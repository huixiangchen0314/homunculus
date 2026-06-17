(ns top.kzre.homunculus.backend.hlsl.methods.assign
  "HLSL :assign 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :assign [node]
  (tmpl/assign (core/emit-node (n/assign-var node))
               (core/emit-node (n/assign-val node))))