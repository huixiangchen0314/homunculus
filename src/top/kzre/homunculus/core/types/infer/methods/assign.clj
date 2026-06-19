(ns top.kzre.homunculus.core.types.infer.methods.assign
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :assign [node context]
  ;; 先推断左侧变量，再推断右侧值，顺序传递上下文
  (let [[_ new-var ctx1] (infer/local-infer (n/assign-var node) context)
        [_ new-val ctx2] (infer/local-infer (n/assign-val node) ctx1)
        ;; 重建赋值节点，类型为 nil（赋值无返回值）
        new-node (n/make-assign new-var new-val
                                (n/attrs node) (n/node-meta node) (n/parent node))]
    (infer/nothing new-node ctx2)))