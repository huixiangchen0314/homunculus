;; ir1/forms/var.clj
(ns top.kzre.homunculus.core.ir1.forms.var
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'var [form]
  (let [[_ var-sym] form]
    (m/->VarNode var-sym nil [] nil)))

(defmethod ir1/build-tree :var [node]
  (let [sym-ir (ir1/->ir1 (:var-sym node))]
    (assoc node :children [sym-ir])))