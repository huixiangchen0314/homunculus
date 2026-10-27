(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'do [form]
  (let [[_ & exprs] form]
    (n/make-do (mapv ir1/->ir1 exprs) (meta form))))