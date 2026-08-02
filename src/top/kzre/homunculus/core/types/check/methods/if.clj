(ns top.kzre.homunculus.core.types.check.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :if [node expected context]
  (let [frontend      (:frontend context)
        truly-type    (when frontend (tp/truly-type frontend))
        test-expected (when truly-type (ty/make-tcon truly-type))]
    (n/make-if (check/check-node (n/if-test node) test-expected context)
               (check/check-node (n/if-then node) expected context)
               (when-let [e (n/if-else node)]
                 (check/check-node e expected context))
               (n/attrs node) (n/node-meta node))))