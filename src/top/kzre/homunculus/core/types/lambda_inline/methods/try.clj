(ns top.kzre.homunculus.core.types.lambda-inline.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :try [node config]
  (n/make-try (inline/eliminate-inline (n/try-body node) config)
              (mapv #(inline/eliminate-inline % config) (n/try-catches node))
              (when-let [finally (n/try-finally node)]
                (inline/eliminate-inline finally config))
              (n/attrs node) (n/node-meta node) (n/parent node)))