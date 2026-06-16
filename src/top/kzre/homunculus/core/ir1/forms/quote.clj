(ns top.kzre.homunculus.core.ir1.forms.quote
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'quote [form]
  (let [[_ expr] form]
    (n/make-quote expr (meta form))))

(defmethod ir1/build-tree :quote [node]
  (n/make-quote (ir1/->ir1 (n/quoted-expr node))
                (n/node-meta node)
                (n/parent node)))