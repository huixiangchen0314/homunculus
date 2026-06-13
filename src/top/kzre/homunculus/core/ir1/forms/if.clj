(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'if [form]
  (let [[_ test then else] form]
    (m/->IfNode test then else (meta form) nil)))

(defmethod ir1/build-tree :if [node]
  (m/->IfNode (ir1/->ir1 (:test node))
              (ir1/->ir1 (:then node))
              (when (:else node) (ir1/->ir1 (:else node)))
              (:meta node)
              (:parent node)))