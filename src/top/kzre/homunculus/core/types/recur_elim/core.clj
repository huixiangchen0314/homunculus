(ns top.kzre.homunculus.core.types.recur-elim.core
  "消除 loop-recur 递归，将 LoopNode 转换为 WhileNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.utils :as u]))

;; ── 辅助：上下文 ──────────────────────────
(defn- make-ctx [var-names result-var recur-flag]
  {:var-names   var-names
   :result-var  result-var
   :recur-flag  recur-flag})


(declare eliminate)

(defn- convert-tail
  "将尾位置的表达式转换，处理 recur 或生成返回/继续语句。"
  [node ctx]
  (let [{:keys [var-names result-var recur-flag]} ctx]
    (case (n/kind node)
      :recur
      (let [args   (n/recur-args node)
            assigns (mapv (fn [var-name arg]
                            (n/make-assign (n/make-variable var-name {} nil )
                                           (eliminate arg)
                                           {} nil))
                          var-names args)
            set-flag (n/make-assign (n/make-variable recur-flag {} nil )
                                    (n/make-literal true {} nil )
                                    {} nil)]
        (n/make-block (conj assigns set-flag) {} nil ))

      :if
      (n/make-if (eliminate (n/if-test node))
                 (convert-tail (n/if-then node) ctx)
                 (when-let [else (n/if-else node)] (convert-tail else ctx))
                 (n/attrs node) (n/node-meta node))

      :block
      (let [exprs     (n/block-exprs node)
            butlast   (butlast exprs)
            last-expr (last exprs)]
        (n/make-block (into (mapv #(eliminate %) butlast)
                            [(convert-tail last-expr ctx)])
                      (n/attrs node) (n/node-meta node)))

      :let
      (n/make-let (mapv (fn [[v e]] [(eliminate v) (eliminate e)])
                        (n/let-bindings node))
                  (convert-tail (n/let-body node) ctx)
                  (n/attrs node) (n/node-meta node))

      :try
      (n/make-try (convert-tail (n/try-body node) ctx)
                  (mapv (fn [c] (n/make-catch (eliminate (n/catch-class c))
                                              (eliminate (n/catch-sym c))
                                              (mapv #(eliminate %) (n/catch-body c))
                                              (n/attrs c) (n/node-meta c)))
                        (n/try-catches node))
                  (when-let [f (n/try-finally node)] (eliminate f))
                  (n/attrs node) (n/node-meta node) )

      ;; 默认：将表达式赋值给 result，并设置 recur-flag = false
      (n/make-block [(n/make-assign (n/make-variable result-var {} nil )
                                    (eliminate node)
                                    {} nil)
                     (n/make-assign (n/make-variable recur-flag {} nil )
                                    (n/make-literal false {} nil )
                                    {} nil )]
                    {} nil )
      )))

;; ── 主转换函数 ────────────────────────────
(defn transform-loop [loop-node]
  (let [bindings   (n/loop-bindings loop-node)
        body       (n/loop-body loop-node)
        var-names  (mapv (fn [[v _]] (n/var-name v)) bindings)
        result-var (u/fresh-name 'result)
        recur-flag (u/fresh-name 'recur?)
        ctx        (make-ctx var-names result-var recur-flag)

        ;; 初始绑定：loop 变量 + result + recur-flag
        loop-bindings (mapv (fn [[var init]]
                              [(n/make-variable (n/var-name var) {} nil )
                               (eliminate init)])
                            bindings)

        all-bindings (into loop-bindings
                           [[(n/make-variable result-var {} nil ) (n/make-literal nil {} nil )]
                            [(n/make-variable recur-flag {} nil ) (n/make-literal true {} nil )]])

        ;; 转换后的循环体
        tail-body  (convert-tail body ctx)
        while-test (n/make-variable recur-flag {} nil )
        while-node (n/make-while while-test tail-body {} nil)
        let-body   (n/make-block [while-node (n/make-variable result-var {} nil )] {} nil )]
    (n/make-let all-bindings let-body {} nil )))

;; ── 分派入口 ──────────────────────────────
(defmulti eliminate (fn [node] (n/kind node)))

(defmethod eliminate :loop [node]
  (transform-loop node))
(defmethod eliminate :recur [node]
  (throw (ex-info "recur outside loop" {:node node})))



(defn elim-fn
  [node env]
  [(eliminate node) env])

(defmethod eliminate :default [node]
  (first (ir2p/reduce-children node elim-fn nil)))

(defn elim-nodes [ir2-roots]
  (mapv eliminate ir2-roots))