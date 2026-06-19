(ns top.kzre.homunculus.backend.hlsl.methods.while
  "HLSL :while 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :while [node context]
  (let [test (core/emit-node (n/while-test node) context)
        body (core/emit-node (n/while-body node) context)]
    [:while test body]))