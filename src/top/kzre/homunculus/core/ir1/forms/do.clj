(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'do [form]
  (let [[_ & exprs] form]
    (ir1/make-node :do :exprs exprs)))

(defmethod ir1/parse-form :do [node]
  (let [expr-irs (mapv ir1/->ir1 (:exprs node))]
    (vec (cons node expr-irs))))