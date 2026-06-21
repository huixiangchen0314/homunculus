(ns top.kzre.homunculus.core.types.ho-elim.core
  "通用高阶函数消除 Pass：通过内联标准库定义并依赖类型信息消除高阶调用。
   不依赖具体函数名，适用于所有用 first/rest/conj 递归定义的高阶函数。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]))

;; ── 多方法分派 ──────────────────────────
(defmulti eliminate-ho
          (fn [node _context] (n/kind node)))

;; ── 辅助函数 ────────────────────────────
(defn- find-definition
  "从上下文中查找符号的定义 IR 节点。"
  [name context]
  (when-let [def-node (get-in context [:definitions name])]
    def-node))

(defn- substitute
  "简单的变量替换（仅处理 VariableNode 的替换），返回新节点。"
  [node subst-map]
  (cond
    (n/variable-node? node)
    (if-let [replacement (get subst-map (n/var-name node))]
      replacement
      node)
    (n/call-node? node)
    (n/make-call (substitute (n/call-fn node) subst-map)
                 (mapv #(substitute % subst-map) (n/call-args node))
                 (n/attrs node) (n/node-meta node) (n/parent node))
    ;; 其他复合节点类似处理，此处省略，实际应完整递归
    :else node))



;; ── 叶子节点 ────────────────────────────
(defmethod eliminate-ho :literal [node _] node)
(defmethod eliminate-ho :variable [node _] node)

;; ── 调用节点（通用内联）────────────────
(defmethod eliminate-ho :call [node context]
  (let [fn-node (n/call-fn node)
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        ;; 检查是否为高阶函数（在符号表中标记或通过已知列表）
        ho-fn? (and fn-name
                    (contains? (get context :ho-fn-set #{}) fn-name))]
    (if ho-fn?
      ;; 获取函数定义（IR2 lambda 节点）
      (if-let [def-node (find-definition fn-name context)]
        (let [lam (n/define-val def-node)           ; lambda 节点
              params (n/lambda-params lam)
              body (n/lambda-body lam)
              args (mapv #(eliminate-ho % context) (n/call-args node))
              ;; 单态化：用实参替换形参
              subst-map (zipmap (map n/var-name params) args)
              inlined-body (substitute body subst-map)]
          (eliminate-ho inlined-body context))
        ;; 找不到定义，保留原调用（可能是递归，由 recur-elim 处理）
        (n/make-call fn-node
                     (mapv #(eliminate-ho % context) (n/call-args node))
                     (n/attrs node) (n/node-meta node) (n/parent node)))
      ;; 普通调用，递归处理子节点
      (n/make-call (eliminate-ho fn-node context)
                   (mapv #(eliminate-ho % context) (n/call-args node))
                   (n/attrs node) (n/node-meta node) (n/parent node)))))

;; ── 其他结构节点递归处理 ───────────────
(defmethod eliminate-ho :if [node context]
  (n/make-if (eliminate-ho (n/if-test node) context)
             (eliminate-ho (n/if-then node) context)
             (when-let [e (n/if-else node)] (eliminate-ho e context))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate-ho :block [node context]
  (n/make-block (mapv #(eliminate-ho % context) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate-ho :let [node context]
  (let [new-bindings (mapv (fn [[v e]] [(eliminate-ho v context) (eliminate-ho e context)])
                           (n/let-bindings node))
        new-body (eliminate-ho (n/let-body node) context)]
    (n/make-let new-bindings new-body
                (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod eliminate-ho :loop [node context]
  ;; 不对 loop 递归体进行内联（会导致无限展开），只处理子节点
  (n/make-loop (mapv (fn [[v e]] [(eliminate-ho v context) (eliminate-ho e context)])
                     (n/loop-bindings node))
               (eliminate-ho (n/loop-body node) context)
               (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate-ho :lambda [node _context]
  ;; 不对 lambda 体进行内联（除非是顶级定义被调用时）
  node)

(defmethod eliminate-ho :define [node context]
  (if-let [val (n/define-val node)]
    (n/make-define (n/define-name node)
                   (eliminate-ho val context)
                   (n/define-doc node)
                   (n/attrs node) (n/node-meta node) (n/parent node))
    node))

(defmethod eliminate-ho :recur [node context]
  (n/make-recur (mapv #(eliminate-ho % context) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod eliminate-ho :default [node _] node)




;; ── 全局入口 ────────────────────────────
(defn process [ir2-roots context]
  (mapv #(eliminate-ho % context) ir2-roots))