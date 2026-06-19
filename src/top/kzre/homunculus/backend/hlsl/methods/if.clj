(ns top.kzre.homunculus.backend.hlsl.methods.if
  "HLSL :if 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :if [node context]
  (let [test (core/emit-node (n/if-test node) context)
        then (core/emit-node (n/if-then node) context)]
    (if-let [else (n/if-else node)]
      [:if test then (core/emit-node else context)]
      [:if test then])))