(ns top.kzre.homunculus.core.types.check.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]))

(defmethod check/check :if [node expected context]
  (let [test-node (check/check (n/if-test node) (t/->TCon :bool) context)
        then-node (check/check (n/if-then node) expected context)
        else-node (when-let [else (n/if-else node)]
                    (check/check else expected context))]
    (n/if-with-children node test-node then-node else-node)))