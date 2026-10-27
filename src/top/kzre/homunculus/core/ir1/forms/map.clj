(ns top.kzre.homunculus.core.ir1.forms.map
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]))

(defmethod ir1/form->node :map [form]
  (let [m (meta form)
        pairs (mapv (fn [[k v]]
                      (m/->Pair (ir1/->ir1 k) (ir1/->ir1 v) m))
                    form)]
    (m/->Map pairs m)))