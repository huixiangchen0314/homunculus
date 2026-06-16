(ns top.kzre.homunculus.core.types.lambda-elim.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :vector [node roots config defs]
  (n/make-vector (mapv #(elim/eliminate % roots config defs) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))