(ns top.kzre.homunculus.core.ir1.forms.if
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'if [form]
  (let [[_ test then else] form]
    (n/make-if test then else (meta form))))

(defmethod ir1/build-tree :if [node]
  (n/make-if (ir1/->ir1 (n/if-test node))
             (ir1/->ir1 (n/if-then node))
             (when-let [else (n/if-else node)] (ir1/->ir1 else))
             (n/node-meta node)
             (n/parent node)))