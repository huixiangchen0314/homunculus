(ns top.kzre.homunculus.backend.hlsl.methods.let
  "HLSL :let 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]))

(defmethod core/emit-node :let [node]
  (throw (ex-info "Let expr is only valid as return-value of function."
                  {:node node})))