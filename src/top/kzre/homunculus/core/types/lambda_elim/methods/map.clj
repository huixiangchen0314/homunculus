(ns top.kzre.homunculus.core.types.lambda-elim.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :map [node roots config defs]
  (n/make-map (mapv #(elim/eliminate % roots config defs) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))