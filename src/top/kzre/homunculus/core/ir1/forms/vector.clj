(ns top.kzre.homunculus.core.ir1.forms.vector
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node :vector [form]
  ;; 将 Clojure 向量中的每个元素先用 form->node 递归转换为 IR1 节点
  (let [raw-items (vec form)
        item-nodes (mapv ir1/form->node raw-items)]
    (m/->VectorNode item-nodes (meta form) nil)))

(defmethod ir1/build-tree :vector [node]
  ;; 递归转换 items 为完整的 IR1 子树，并设置 parent
  (m/->VectorNode (mapv ir1/->ir1 (:items node))
                  (:meta node)
                  (:parent node)))