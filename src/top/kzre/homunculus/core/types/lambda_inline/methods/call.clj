(ns top.kzre.homunculus.core.types.lambda-inline.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :call [node config]
  (n/make-call (inline/eliminate-inline (n/call-fn node) config)
               (mapv #(inline/eliminate-inline % config) (n/call-args node))
               (n/attrs node) (n/node-meta node) (n/parent node)))