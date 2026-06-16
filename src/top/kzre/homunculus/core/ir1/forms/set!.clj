(ns top.kzre.homunculus.core.ir1.forms.set!
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'set! [form]
  (let [[_ var-sym val] form]
    (n/make-set! var-sym val (meta form))))

(defmethod ir1/build-tree :set! [node]
  (n/make-set! (ir1/->ir1 (n/set-var node))
               (ir1/->ir1 (n/set-val node))
               (n/node-meta node)
               (n/parent node)))