(ns top.kzre.homunculus.core.types.check.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :if [node expected context]
  (let [test-node (check/check-node (n/if-test node) (ty/make-tcon :bool) context)
        then-node (check/check-node (n/if-then node) expected context)
        else-node (when-let [else (n/if-else node)]
                    (check/check-node else expected context))]
    (n/make-if test-node then-node else-node
               (n/attrs node) (n/node-meta node) (n/parent node))))