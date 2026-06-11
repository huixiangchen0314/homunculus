;; ir1/forms/do.clj
(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'do [form]
  (let [[_ & exprs] form]
    (m/->DoNode (vec exprs) nil [] nil)))

(defmethod ir1/build-tree :do [node]
  (let [expr-nodes (mapv ir1/->ir1 (:exprs node))]
    (assoc node :children expr-nodes)))