(ns top.kzre.homunculus.core.types.lambda-elim.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :try [node roots config defs]
  (n/make-try (elim/eliminate (n/try-body node) roots config defs)
              (mapv #(elim/eliminate % roots config defs) (n/try-catches node))
              (when-let [finally (n/try-finally node)]
                (elim/eliminate finally roots config defs))
              (n/attrs node) (n/node-meta node) (n/parent node)))