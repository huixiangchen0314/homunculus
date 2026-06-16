(ns top.kzre.homunculus.core.types.lambda-inline.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :map [node config]
  (n/make-map (mapv #(inline/eliminate-inline % config) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))