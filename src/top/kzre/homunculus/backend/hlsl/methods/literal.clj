(ns top.kzre.homunculus.backend.hlsl.methods.literal
  "HLSL :literal 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :literal [node]
  (tmpl/hlsl-literal (n/lit-val node)))