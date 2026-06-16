(ns top.kzre.homunculus.core.types.recur-elim.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :vector [node]
  (n/make-vector (mapv rec/eliminate (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))