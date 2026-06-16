(ns top.kzre.homunculus.core.types.recur-elim.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :if [node]
  (n/make-if (rec/eliminate (n/if-test node))
             (rec/eliminate (n/if-then node))
             (when-let [else (n/if-else node)]
               (rec/eliminate else))
             (n/attrs node) (n/node-meta node) (n/parent node)))