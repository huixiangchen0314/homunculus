(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'if [form env]
  (let [[_ test then else] form
        [test-node env'] (ir1/lower-node test env)
        [then-node _]    (ir1/lower-node then env')
        [else-node _]    (when else (ir1/lower-node else env'))]
    [(n/make-if test-node then-node else-node (meta form)) env']))