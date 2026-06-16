(ns top.kzre.homunculus.core.types.lambda-inline.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :loop [node config]
  (let [new-bindings (mapv (fn [[v e]]
                             [(inline/eliminate-inline v config)
                              (inline/eliminate-inline e config)])
                           (n/loop-bindings node))
        new-body     (inline/eliminate-inline (n/loop-body node) config)]
    (n/make-loop new-bindings new-body
                 (n/attrs node) (n/node-meta node) (n/parent node))))