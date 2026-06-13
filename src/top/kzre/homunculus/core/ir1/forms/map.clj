(ns top.kzre.homunculus.core.ir1.forms.map
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node :map [form]
  ;; 将 Clojure map 中的键值对分别转换为 IR1 节点（交替存储）
  (let [pairs (vec form)
        ;; pairs 现在是扁平的 [key1 val1 key2 val2 ...]
        node-pairs (mapv ir1/form->node pairs)]
    (m/->MapNode node-pairs (meta form) nil)))

(defmethod ir1/build-tree :map [node]
  ;; 递归转换所有键值对为 IR1 子树
  (m/->MapNode (mapv ir1/->ir1 (:pairs node))
               (:meta node)
               (:parent node)))