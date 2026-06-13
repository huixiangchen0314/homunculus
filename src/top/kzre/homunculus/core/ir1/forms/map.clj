(ns top.kzre.homunculus.core.ir1.forms.map
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node :map [form]
  ;; 不递归，直接存储原始 Clojure map 元素（交替键值对），由 build-tree 递归转换
  (m/->MapNode (vec form) (meta form) nil))

(defmethod ir1/build-tree :map [node]
  (m/->MapNode (mapv ir1/->ir1 (:pairs node))
               (:meta node)
               (:parent node)))