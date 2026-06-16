(ns top.kzre.homunculus.core.ir1.forms.vector
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node :vector [form]
  (n/make-vector (vec form) (meta form)))

(defmethod ir1/build-tree :vector [node]
  (n/make-vector (mapv ir1/->ir1 (n/vec-items node))
                 (n/node-meta node)
                 (n/parent node)))