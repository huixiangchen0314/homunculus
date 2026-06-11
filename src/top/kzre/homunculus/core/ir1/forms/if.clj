;; ir1/forms/if.clj
(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'if [form]
  (let [[_ test then else] form]
    (m/->IfNode test then else nil [] nil)))

(defmethod ir1/build-tree :if [node]
  (let [test-node (ir1/->ir1 (:test node))
        then-node (ir1/->ir1 (:then node))
        else-node (when (:else node) (ir1/->ir1 (:else node)))
        children (if else-node [test-node then-node else-node] [test-node then-node])]
    (assoc node :children children)))