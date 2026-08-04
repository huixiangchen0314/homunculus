(ns top.kzre.homunculus.core.ir1.forms.call
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node :call [form]
  (let [[op & args] form]
    (n/make-call (ir1/->ir1 op)
                 (mapv ir1/->ir1 args)
                 (meta form))))
