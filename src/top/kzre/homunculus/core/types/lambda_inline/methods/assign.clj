(ns top.kzre.homunculus.core.types.lambda-inline.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :assign [node config]
  (n/make-assign (inline/eliminate-inline (n/assign-var node) config)
                 (inline/eliminate-inline (n/assign-val node) config)
                 (n/attrs node) (n/node-meta node) (n/parent node)))