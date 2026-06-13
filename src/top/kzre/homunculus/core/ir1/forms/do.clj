(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'do [form]
  (let [[_ & exprs] form]
    (m/->DoNode (vec exprs) (meta form) nil)))

(defmethod ir1/build-tree :do [node]
  (m/->DoNode (mapv ir1/->ir1 (:exprs node)) (:meta node) (:parent node)))