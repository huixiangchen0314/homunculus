(ns top.kzre.homunculus.core.types.lambda-elim.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :if [node config env]
  (let [[test test-defs] (elim/eliminate (n/if-test node) config env)
        [then then-defs] (elim/eliminate (n/if-then node) config env)
        [else else-defs] (if-let [e (n/if-else node)]
                           (elim/eliminate e config env)
                           [nil []])]
    [(n/make-if test then else (n/attrs node) (n/node-meta node) (n/parent node))
     (into test-defs (into then-defs else-defs))]))