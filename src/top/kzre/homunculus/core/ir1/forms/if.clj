(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'if [form]
  (let [[_ test then else] form]
    (n/make-if (ir1/->ir1 test)
               (ir1/->ir1 then)
               (when else (ir1/->ir1 else))
               (meta form))))