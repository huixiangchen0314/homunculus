(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'if [form]
  (let [[_ test then else] form]
    (ir1/make-node :if :test test :then then :else else)))

(defmethod ir1/parse-form :if [node]
  (let [test-ir (ir1/->ir1 (:test node))
        then-ir (ir1/->ir1 (:then node))
        else-ir (when (:else node) (ir1/->ir1 (:else node)))]
    (vec (remove nil? (list* node test-ir then-ir (when else-ir [else-ir]))))))