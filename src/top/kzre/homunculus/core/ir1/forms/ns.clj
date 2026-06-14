(ns top.kzre.homunculus.core.ir1.forms.ns
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'ns* [form]
  (let [[_ name docstring attr-map references] form]
    (m/->NsNode name docstring attr-map references (meta form) nil)))

(defmethod ir1/build-tree :ns [node]
  ;; ns 节点不递归构建子节点，保留原数据
  node)