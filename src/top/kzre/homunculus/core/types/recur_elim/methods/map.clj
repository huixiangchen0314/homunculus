(ns top.kzre.homunculus.core.types.recur-elim.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :map [node]
  (n/make-map (mapv rec/eliminate (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))