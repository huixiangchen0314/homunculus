(ns top.kzre.homunculus.backend.hlsl.methods.variable
  "HLSL :variable 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :variable [node]
  (tmpl/var-ref (name (n/var-name node))))