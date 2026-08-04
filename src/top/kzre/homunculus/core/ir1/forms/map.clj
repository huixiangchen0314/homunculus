(ns top.kzre.homunculus.core.ir1.forms.map
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]))

(defmethod ir1/form->node :map [form]
  (let [meta (meta form)
        pairs (for [[k v] form]
                (m/->Pair (ir1/->ir1 k) (ir1/->ir1 v) meta))]
    (m/->Map (vec pairs) meta)))

(defmethod ir1/build-tree :map [node] node)