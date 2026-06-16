(ns top.kzre.homunculus.core.types.lambda-elim.methods.if
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :if [node roots config defs]
  (n/make-if (elim/eliminate (n/if-test node) roots config defs)
             (elim/eliminate (n/if-then node) roots config defs)
             (when-let [else (n/if-else node)]
               (elim/eliminate else roots config defs))
             (n/attrs node) (n/node-meta node) (n/parent node)))