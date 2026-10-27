(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'do [form env]
  (let [[_ & exprs] form
        [nodes env'] (ir1/lower-seq exprs env)]
    [(n/make-do nodes (meta form)) env']))