(ns top.kzre.homunculus.core.types.lambda-inline.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :if [node config]
  (n/make-if (inline/eliminate-inline (n/if-test node) config)
             (inline/eliminate-inline (n/if-then node) config)
             (when-let [else (n/if-else node)]
               (inline/eliminate-inline else config))
             (n/attrs node) (n/node-meta node) (n/parent node)))