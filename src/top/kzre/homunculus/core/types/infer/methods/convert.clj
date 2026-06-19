(ns top.kzre.homunculus.core.types.infer.methods.convert
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :convert [node context]
  ;; 推断被转换的表达式，累积上下文
  (let [[_ new-expr ctx1] (infer/local-infer (n/convert-expr node) context)
        ;; 重建 convert 节点，保留原有类型转换信息
        new-node (n/make-convert new-expr
                                 (n/convert-src-ty node)
                                 (n/convert-dst-ty node)
                                 (n/convert-cost node)
                                 (n/attrs node) (n/node-meta node) (n/parent node))]
    ;; convert 节点本身无类型，返回新节点及传递后的上下文
    (infer/nothing new-node ctx1)))