(ns top.kzre.homunculus.core.ir1.forms.var-set
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'var [form]
  (let [[_ var-sym] form]
    (ir1/make-node :var :var-sym var-sym)))

(defmethod ir1/parse-form :var [node]
  (let [sym-ir (ir1/->ir1 (:var-sym node))]
    [node sym-ir]))

(defmethod ir1/form->node 'set! [form]
  (let [[_ var-sym val] form]
    (ir1/make-node :set! :var var-sym :val val)))

(defmethod ir1/parse-form :set! [node]
  (let [var-ir (ir1/->ir1 (:var node))
        val-ir (ir1/->ir1 (:val node))]
    [node var-ir val-ir]))