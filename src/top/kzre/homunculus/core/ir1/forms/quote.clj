(ns top.kzre.homunculus.core.ir1.forms.quote
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'quote [form]
  (let [[_ expr] form]
    (ir1/make-node :quote :expr expr)))

(defmethod ir1/parse-form :quote [node]
  (let [expr-ir (ir1/->ir1 (:expr node))]
    [node expr-ir]))