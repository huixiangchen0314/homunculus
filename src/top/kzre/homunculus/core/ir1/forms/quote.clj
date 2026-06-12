(ns top.kzre.homunculus.core.ir1.forms.quote
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'quote [form]
  (let [[_ expr] form]
    (m/->QuoteNode expr nil nil)))

(defmethod ir1/build-tree :quote [node]
  (m/->QuoteNode (ir1/->ir1 (:expr node)) (:meta node) (:parent node)))