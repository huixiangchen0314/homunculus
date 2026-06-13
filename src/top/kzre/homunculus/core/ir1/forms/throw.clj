(ns top.kzre.homunculus.core.ir1.forms.throw
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'throw [form]
  (let [[_ expr] form]
    (m/->ThrowNode expr (meta form) nil)))

(defmethod ir1/build-tree :throw [node]
  (m/->ThrowNode (ir1/->ir1 (:expr node)) (:meta node) (:parent node)))