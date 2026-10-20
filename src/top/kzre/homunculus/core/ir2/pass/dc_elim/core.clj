(ns top.kzre.homunculus.core.ir2.pass.dc-elim.core
  "死代码消除 Pass：根据配置消除未引用的高阶函数、内联函数和多态函数定义。"
  (:require [top.kzre.homunculus.core.ir2.ast :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.type :as ty]
            [top.kzre.homunculus.core.ir2.pass.mark-ho :as mark-ho]))

(defn make-context
  "构造 DCE Pass 的配置上下文。
   compile-ctx 为编译上下文（未使用，保留兼容），opts 为可选配置键：
   :dce-eliminate-ho?           (默认 true)
   :dce-eliminate-inline?        (默认 true)
   :dce-eliminate-polymorphic?  (默认 true)"
  [compile-ctx & {:keys [dce-eliminate-ho?
                         dce-eliminate-inline?
                         dce-eliminate-polymorphic?]
                  :or {dce-eliminate-ho? true
                       dce-eliminate-inline? true
                       dce-eliminate-polymorphic? true}}]
  {:dce-eliminate-ho? dce-eliminate-ho?
   :dce-eliminate-inline? dce-eliminate-inline?
   :dce-eliminate-polymorphic? dce-eliminate-polymorphic?})


(defn eliminate-ho-defs
  "从 IR2 根列表中移除所有标记为 :ho? true 的 define 节点。
   当 :dce-eliminate-ho? 为 false 时，不做任何过滤。"
  [ir2-roots context]
  (if (get context :dce-eliminate-ho? true)
    (filterv (fn [root]
               (if (n/define-node? root)
                 (let [val-node (n/define-val root)]
                   (if (= :lambda (ir2/kind val-node))
                     ;; 有类型标记就查询类型推断，否则走自由变量分析
                     (if-let [t (ty/get-type root)]
                       (ty/concrete? t)
                       (mark-ho/high-order? val-node))
                     true)
                   )
                 true))
             ir2-roots)
    ir2-roots))

(defn eliminate-inline-defs
  "移除标记为 :inline true 的 define 节点。
   当 :dce-eliminate-inline? 为 false 时，不做任何过滤。"
  [ir2-roots context]
  (if (get context :dce-eliminate-inline? true)
    (filterv (fn [root]
               (if (n/define-node? root)
                 (not (-> root n/attrs :inline))
                 true))
             ir2-roots)
    ir2-roots))

(defn eliminate-polymorphic-defs
  "移除类型中包含类型变量（非具体）的 define 节点。
   当 :dce-eliminate-polymorphic? 为 false 时，不做任何过滤。"
  [ir2-roots dce-ctx]
  (if (:dce-eliminate-polymorphic? dce-ctx)
    (filterv (fn [root]
               (if (n/define-node? root)
                 (if-let [ty (ty/get-type root)]  ;; 已有推断类型
                   (ty/concrete? ty)               ;; 具体类型才保留
                   true)                           ;; 无类型信息则保守保留
                 true))
             ir2-roots)
    ir2-roots))

(defn eliminate-all
  "一次性移除所有标记为 ho? / inline / polymorphic 的 define 节点。"
  [ir2-roots context]
  (-> ir2-roots
      (eliminate-ho-defs context)
      (eliminate-inline-defs context)
      (eliminate-polymorphic-defs context)))