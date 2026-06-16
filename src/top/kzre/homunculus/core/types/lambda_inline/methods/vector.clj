(ns top.kzre.homunculus.core.types.lambda-inline.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :vector [node config]
  (n/make-vector (mapv #(inline/eliminate-inline % config) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))