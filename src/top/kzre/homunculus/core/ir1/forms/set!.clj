;; ir1/forms/set!.clj
(ns top.kzre.homunculus.core.ir1.forms.set!
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'set! [form]
  (let [[_ var-sym val] form]
    (m/->SetNode var-sym val nil [] nil)))

(defmethod ir1/build-tree :set! [node]
  (let [var-ir (ir1/->ir1 (:var node))
        val-ir (ir1/->ir1 (:val node))]
    (assoc node :children [var-ir val-ir])))