(ns top.kzre.homunculus.core.ir1.forms.var
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'var [form]
  (let [[_ var-sym] form]
    (n/make-var var-sym (meta form))))

(defmethod ir1/build-tree :var [node]
  (n/make-var (ir1/->ir1 (n/var-sym node))
              (n/node-meta node)
              (n/parent node)))