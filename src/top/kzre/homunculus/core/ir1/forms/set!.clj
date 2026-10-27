(ns top.kzre.homunculus.core.ir1.forms.set!
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'set! [form env]
  (let [[_ var val] form
        [var-node env']  (ir1/lower-node var env)
        [val-node env''] (ir1/lower-node val env')]
    [(n/make-set! var-node val-node (meta form)) env'']))