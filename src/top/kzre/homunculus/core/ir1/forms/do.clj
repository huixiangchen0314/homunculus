(ns top.kzre.homunculus.core.ir1.forms.do
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'do [form]
  (let [[_ & exprs] form]
    (n/make-do (vec exprs) (meta form))))

(defmethod ir1/build-tree :do [node]
  (n/make-do (mapv ir1/->ir1 (n/do-exprs node))
             (n/node-meta node)
             (n/parent node)))