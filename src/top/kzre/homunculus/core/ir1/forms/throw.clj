;; ir1/forms/throw.clj
(ns top.kzre.homunculus.core.ir1.forms.throw
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'throw [form]
  (let [[_ expr] form]
    (m/->ThrowNode expr nil [] nil)))

(defmethod ir1/build-tree :throw [node]
  (let [expr-ir (ir1/->ir1 (:expr node))]
    (assoc node :children [expr-ir])))