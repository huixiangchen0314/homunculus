(ns top.kzre.homunculus.core.ir1.forms.set!
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'set! [form]
  (let [[_ var val] form]
    (n/make-set! (ir1/->ir1 var) (ir1/->ir1 val) (meta form))))