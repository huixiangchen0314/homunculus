(ns top.kzre.homunculus.core.ir1.forms.vector
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node :vector [form]
  ;; 不递归，直接存储原始 Clojure 向量元素，由 build-tree 递归转换
  (m/->VectorNode (vec form) (meta form) nil))

(defmethod ir1/build-tree :vector [node]
  (m/->VectorNode (mapv ir1/->ir1 (:items node))
                  (:meta node)
                  (:parent node)))