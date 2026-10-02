(ns top.kzre.homunculus.core.ir2.pass.recur-elim.core
  "消除 loop-recur 递归，将 LoopNode 转换为 WhileNode。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.transform.rename.rename :as rename]
    [top.kzre.homunculus.core.ir2.pass.recur-elim.env :as p]))

(declare elim-node*)

(defn- convert-tail
  "将尾位置的表达式转换，处理 recur 或生成返回/继续语句。"
  [node env]
  (let [var-names  (p/loop-var-names env)
        result-var (p/result-var env)
        recur-flag (p/recur-flag env)]
    (case (n/kind node)
      :recur
      (let [args   (n/recur-args node)
            assigns (mapv (fn [var-name arg]
                            (n/make-assign (n/make-variable var-name {} nil)
                                           (elim-node* arg)
                                           {} nil))
                          var-names args)
            set-flag (n/make-assign (n/make-variable recur-flag {} nil)
                                    (n/make-literal true {} nil)
                                    {} nil)]
        (n/make-block (conj assigns set-flag) {} nil))

      :if
      (n/make-if (elim-node* (n/if-test node))
                 (convert-tail (n/if-then node) env)
                 (when-let [else (n/if-else node)] (convert-tail else env))
                 (n/attrs node) (n/node-meta node))

      :block
      (let [exprs     (n/block-exprs node)
            butlast   (butlast exprs)
            last-expr (last exprs)]
        (n/make-block (into (mapv #(elim-node* %) butlast)
                            [(convert-tail last-expr env)])
                      (n/attrs node) (n/node-meta node)))

      :let
      (let [bindings     (n/let-bindings node)
            new-bindings (mapv (fn [b]
                                 (assoc b
                                   :var (elim-node* (:var b))
                                   :val (elim-node* (:val b))))
                               bindings)]
        (n/make-let new-bindings
                    (convert-tail (n/let-body node) env)
                    (n/attrs node) (n/node-meta node)))

      :try
      (n/make-try (convert-tail (n/try-body node) env)
                  (mapv (fn [c] (n/make-catch (elim-node* (n/catch-class c))
                                              (elim-node* (n/catch-sym c))
                                              (mapv #(elim-node* %) (n/catch-body c))
                                              (n/attrs c) (n/node-meta c)))
                        (n/try-catches node))
                  (when-let [f (n/try-finally node)] (elim-node* f))
                  (n/attrs node) (n/node-meta node))

      ;; 默认：将表达式赋值给 result，并设置 recur-flag = false
      (n/make-block [(n/make-assign (n/make-variable result-var {} nil)
                                    (elim-node* node)
                                    {} nil)
                     (n/make-assign (n/make-variable recur-flag {} nil)
                                    (n/make-literal false {} nil)
                                    {} nil)]
                    {} nil))))

(defn transform-loop [loop-node]
  (let [bindings  (n/loop-bindings loop-node)
        body      (n/loop-body loop-node)
        var-names (mapv #(:name (:var %)) bindings)
        env       (p/make-env var-names)

        ;; 初始绑定：保留 loop 变量节点，消除初始化表达式
        loop-bindings (mapv (fn [b]
                              (assoc b :val (elim-node* (:val b))))
                            bindings)

        result-binding (ir2/->Binding
                         (n/make-variable (p/result-var env) {} nil)
                         (n/make-literal nil {} nil) {} nil)
        recur-binding  (ir2/->Binding
                         (n/make-variable (p/recur-flag env) {} nil)
                         (n/make-literal true {} nil) {} nil)

        all-bindings (into (vec loop-bindings) [result-binding recur-binding])

        tail-body  (convert-tail body env)
        while-test (n/make-variable (p/recur-flag env) {} nil)
        while-node (n/make-while while-test tail-body {} nil)
        let-body   (n/make-block [while-node
                                  (n/make-variable (p/result-var env) {} nil)]
                                 {} nil)]
    (n/make-let all-bindings let-body {} nil)))

;; ── 分派入口 ──────────────────────────────
(defmulti elim-node* (fn [node] (n/kind node)))

(defmethod elim-node* :loop [node]
  (rename/rename-node (transform-loop node)))

(defmethod elim-node* :recur [node]
  (throw (ex-info "recur outside loop" {:node node})))

(defn elim-fn
  [node env]
  [(elim-node* node) env])

(defmethod elim-node* :default [node]
  (first (ir2/reduce-children node elim-fn nil)))

(defn elim [ir2-roots]
  (mapv elim-node* ir2-roots))