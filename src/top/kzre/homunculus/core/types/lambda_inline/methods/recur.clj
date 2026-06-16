(ns top.kzre.homunculus.core.types.lambda-inline.methods.recur
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :recur [node config]
  (n/make-recur (mapv #(inline/eliminate-inline % config) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))