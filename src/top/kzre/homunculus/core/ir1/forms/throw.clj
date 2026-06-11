(ns top.kzre.homunculus.core.ir1.forms.throw
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'throw [form]
  (let [[_ expr] form]
    (ir1/make-node :throw :expr expr)))

(defmethod ir1/parse-form :throw [node]
  (let [expr-ir (ir1/->ir1 (:expr node))]
    [node expr-ir]))