(ns top.kzre.homunculus.core.ir1.forms.set!
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'set! [form]
  (let [[_ var-sym val] form]
    (m/->SetNode var-sym val (meta form) nil)))

(defmethod ir1/build-tree :set! [node]
  (m/->SetNode (ir1/->ir1 (:var node)) (ir1/->ir1 (:val node)) (:meta node) (:parent node)))