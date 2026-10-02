(ns top.kzre.homunculus.core.ir2.pass.infer.methods.assign
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :assign [node env]
  ;; 先推断左侧变量，再推断右侧值，顺序传递环境
  (let [[_ new-var env1] (infer/infer-node* (n/assign-var node) env)
        [_ new-val env2] (infer/infer-node* (n/assign-val node) env1)
        ;; 重建赋值节点，类型为 nil（赋值无返回值）
        new-node (n/make-assign new-var new-val
                                (n/attrs node)
                                (n/node-meta node))]
    (infer/nothing new-node env2)))