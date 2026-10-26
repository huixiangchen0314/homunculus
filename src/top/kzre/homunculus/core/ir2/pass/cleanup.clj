(ns top.kzre.homunculus.core.ir2.pass.cleanup
  "结构清理 Pass：清理后端不兼容的语言结构。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.mark-trait :as mark-trait]
            [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn elim-high-order
  "从 IR2 根列表中移除所有高阶函数"
  [ir2-roots ]
  (filterv (fn [root]
             (if (n/define-node? root)
               (not (mark-trait/high-order? root))
               true))
           ir2-roots))

(defn elim-inline
  "移除标记为 内联函数定义"
  [ir2-roots]
  (filterv (fn [root]
             (if (n/define-node? root)
               (not (mark-trait/inline? root))
               true))
           ir2-roots))

(defn elim-polymorphic
  "移除类型中包含类型变量（非具体）的 define 节点。"
  [ir2-roots ]
  (filterv (fn [root]
             (if (n/define-node? root)
               (if-let [ty (ty/get-type root)]  ;; 已有推断类型
                 (not (ty/concrete? ty))        ;; 具体类型才保留
                 true)                           ;; 无类型信息则保守保留
               true))
           ir2-roots))