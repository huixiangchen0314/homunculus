(ns top.kzre.homunculus.backend.hlsl.methods.assign
  "HLSL :assign 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :assign [node context]
  (let [target (core/emit-node (n/assign-var node) context)
        value  (core/emit-node (n/assign-val node) context)]
    [:assign target value]))