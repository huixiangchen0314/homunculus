(ns top.kzre.homunculus.core.ir1.forms.map
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(ns top.kzre.homunculus.core.ir1.forms.map
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node :map [form]
  ;; 将 Clojure map 字面量 {:a 1 :b 2} 转换为交替键值序列 [:a 1 :b 2]
  (let [pairs (mapcat (fn [[k v]] [k v]) form)]
    (m/->MapNode (vec pairs) (meta form) nil)))

(defmethod ir1/build-tree :map [node]
  ;; 对交替序列中每个元素递归转为 IR1 节点
  (m/->MapNode (mapv ir1/->ir1 (:pairs node))
               (:meta node)
               (:parent node)))