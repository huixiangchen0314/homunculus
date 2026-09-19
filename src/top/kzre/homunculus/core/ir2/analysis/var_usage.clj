(ns top.kzre.homunculus.core.ir2.analysis.var-usage
  "变量使用分析。

   分析一个变量在子树中的使用模式：
   - 是否只在调用位置出现（可作为内联前提）
   - 收集所有调用位置的节点

   只读。不产出新 IR。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]))

;; ═══════════════════════════════════════════════
;; 约束
;; ═══════════════════════════════════════════════

(defn- ensure-ir2!
  [node where]
  (when-not (ir2/ir2? node)
    (throw (ex-info (str where " expects an IR2 node")
                    {:node node
                     :type (type node)}))))

;; ═══════════════════════════════════════════════
;; 非调用使用
;; ═══════════════════════════════════════════════

(declare non-call-usage?)

(defn- call-node-non-call-usage?
  "在 :call 节点中，判断目标变量是否有非调用使用。
   fn 位置出现目标变量 = 调用（合法）；
   fn 位置出现其他表达式 = 递归（表达式内可能含目标）；
   args 位置出现目标变量 = 非调用使用。"
  [node var-name]
  (let [fn-node  (n/call-fn node)
        args     (n/call-args node)
        fn-target? (and (ir2/ir2? fn-node)
                        (= :variable (n/kind fn-node))
                        (= (n/var-name fn-node) var-name))]
    (or (and (not fn-target?)
             (non-call-usage? fn-node var-name))
        (some #(non-call-usage? % var-name) args))))

(defn non-call-usage?
  "判断 var-name 在 node 子树中是否有非调用使用。

   调用使用 = 出现在 :call 节点的 fn 位置。
   其他位置（实参、赋值、初始化等）= 非调用使用。

   返回 true 表示内联不安全——目标变量在别处被引用。"
  [node var-name]
  (ensure-ir2! node "non-call-usage?")
  (case (n/kind node)
    :variable
    (= (n/var-name node) var-name)

    :call
    (call-node-non-call-usage? node var-name)

    ;; 默认：递归子节点
    (boolean (some #(non-call-usage? % var-name) (n/children node)))))

;; ═══════════════════════════════════════════════
;; 调用点收集
;; ═══════════════════════════════════════════════

(defn call-sites
  "收集 node 子树中所有形如 (var-name ...) 的 :call 节点。

   返回 :call 节点的向量——每个都是目标变量作为 fn 位置的调用。"
  [node var-name]
  (ensure-ir2! node "call-sites")
  (let [child-sites
        (into [] (mapcat #(call-sites % var-name)) (n/children node))
        this-site?
        (and (= :call (n/kind node))
             (let [fn-node (n/call-fn node)]
               (and (ir2/ir2? fn-node)
                    (= :variable (n/kind fn-node))
                    (= (n/var-name fn-node) var-name))))]
    (if this-site?
      (into [node] child-sites)
      child-sites)))