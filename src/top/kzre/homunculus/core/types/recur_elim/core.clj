(ns top.kzre.homunculus.core.types.recur-elim.core
  "消除 loop-recur 递归，将 LoopNode 转换为 WhileNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.utils :as u]))

;; ── 辅助：上下文 ──────────────────────────
(defn- make-ctx [var-names result-var recur-flag]
  {:var-names   var-names
   :result-var  result-var
   :recur-flag  recur-flag})

;; ── 非尾位置的表达式转换 ─────────────────
(defn- convert-expr
  "将非尾位置的表达式递归转换，recur 出现在此处直接报错。"
  [node ctx]
  (case (n/kind node)
    :recur   (throw (ex-info "recur used outside tail position" {:node node}))
    :if      (n/make-if (convert-expr (n/if-test node) ctx)
                        (convert-expr (n/if-then node) ctx)
                        (when-let [else (n/if-else node)] (convert-expr else ctx))
                        (n/attrs node) (n/node-meta node) (n/parent node))
    :let     (n/make-let (mapv (fn [[v e]] [(convert-expr v ctx) (convert-expr e ctx)])
                               (n/let-bindings node))
                         (convert-expr (n/let-body node) ctx)
                         (n/attrs node) (n/node-meta node) (n/parent node))
    :block   (n/make-block (mapv #(convert-expr % ctx) (n/block-exprs node))
                           (n/attrs node) (n/node-meta node) (n/parent node))
    :call    (n/make-call (convert-expr (n/call-fn node) ctx)
                          (mapv #(convert-expr % ctx) (n/call-args node))
                          (n/attrs node) (n/node-meta node) (n/parent node))
    :assign  (n/make-assign (convert-expr (n/assign-var node) ctx)
                            (convert-expr (n/assign-val node) ctx)
                            (n/attrs node) (n/node-meta node) (n/parent node))
    :vector  (n/make-vector (mapv #(convert-expr % ctx) (n/vector-items node))
                            (n/attrs node) (n/node-meta node) (n/parent node))
    :map     (n/make-map (mapv #(convert-expr % ctx) (n/map-kvs node))
                         (n/attrs node) (n/node-meta node) (n/parent node))
    :member-access (n/make-member-access (convert-expr (n/access-target node) ctx)
                                         (n/access-member node)
                                         (mapv #(convert-expr % ctx) (n/access-args node))
                                         (n/node-meta node) (n/parent node))
    :try     (n/make-try (convert-expr (n/try-body node) ctx)
                         (mapv (fn [c] (n/make-catch (convert-expr (n/catch-class c) ctx)
                                                     (convert-expr (n/catch-sym c) ctx)
                                                     (mapv #(convert-expr % ctx) (n/catch-body c))
                                                     (n/attrs c) (n/node-meta c) (n/parent c)))
                               (n/try-catches node))
                         (when-let [f (n/try-finally node)] (convert-expr f ctx))
                         (n/attrs node) (n/node-meta node) (n/parent node))
    :throw   (n/make-throw (convert-expr (n/throw-expr node) ctx)
                           (n/attrs node) (n/node-meta node) (n/parent node))
    ;; 其他叶子节点（literal、variable 等）直接返回
    node))

;; ── 尾位置的表达式转换 ────────────────────
(defn- convert-tail
  "将尾位置的表达式转换，处理 recur 或生成返回/继续语句。"
  [node ctx]
  (let [{:keys [var-names result-var recur-flag]} ctx]
    (case (n/kind node)
      :recur
      (let [args   (n/recur-args node)
            assigns (mapv (fn [var-name arg]
                            (n/make-assign (n/make-variable var-name nil nil)
                                           (convert-expr arg ctx)
                                           nil nil nil))
                          var-names args)
            set-flag (n/make-assign (n/make-variable recur-flag nil nil)
                                    (n/make-literal true nil nil)
                                    nil nil nil)]
        (n/make-block (conj assigns set-flag) nil nil nil))

      :if
      (n/make-if (convert-expr (n/if-test node) ctx)
                 (convert-tail (n/if-then node) ctx)
                 (when-let [else (n/if-else node)] (convert-tail else ctx))
                 (n/attrs node) (n/node-meta node) (n/parent node))

      :block
      (let [exprs     (n/block-exprs node)
            butlast   (butlast exprs)
            last-expr (last exprs)]
        (n/make-block (into (mapv #(convert-expr % ctx) butlast)
                            [(convert-tail last-expr ctx)])
                      (n/attrs node) (n/node-meta node) (n/parent node)))

      :let
      (n/make-let (mapv (fn [[v e]] [(convert-expr v ctx) (convert-expr e ctx)])
                        (n/let-bindings node))
                  (convert-tail (n/let-body node) ctx)
                  (n/attrs node) (n/node-meta node) (n/parent node))

      :try
      (n/make-try (convert-tail (n/try-body node) ctx)
                  (mapv (fn [c] (n/make-catch (convert-expr (n/catch-class c) ctx)
                                              (convert-expr (n/catch-sym c) ctx)
                                              (mapv #(convert-expr % ctx) (n/catch-body c))
                                              (n/attrs c) (n/node-meta c) (n/parent c)))
                        (n/try-catches node))
                  (when-let [f (n/try-finally node)] (convert-expr f ctx))
                  (n/attrs node) (n/node-meta node) (n/parent node))

      ;; 默认：将表达式赋值给 result，并设置 recur-flag = false
      (n/make-block [(n/make-assign (n/make-variable result-var nil nil)
                                    node
                                    nil nil nil)
                     (n/make-assign (n/make-variable recur-flag nil nil)
                                    (n/make-literal false nil nil)
                                    nil nil nil)]
                    nil nil nil))))

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
                              [(n/make-variable (n/var-name var) nil nil)
                               (convert-expr init ctx)])
                            bindings)

        ;; 优化：result 初始化为 nil，避免虚假类型
        all-bindings (into loop-bindings
                           [[(n/make-variable result-var nil nil) (n/make-literal nil nil nil)]
                            [(n/make-variable recur-flag nil nil) (n/make-literal true nil nil)]])

        ;; 转换后的循环体
        tail-body  (convert-tail body ctx)
        while-test (n/make-variable recur-flag nil nil)
        while-node (n/make-while while-test tail-body nil nil nil)
        let-body   (n/make-block [while-node (n/make-variable result-var nil nil)]
                                 nil nil nil)]
    (n/make-let all-bindings let-body nil nil nil)))

;; ── 分派入口 ──────────────────────────────
(defmulti eliminate (fn [node] (n/kind node)))
