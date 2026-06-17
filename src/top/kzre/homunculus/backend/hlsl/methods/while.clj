(ns top.kzre.homunculus.backend.hlsl.methods.while
  "HLSL :while 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :while [node]
  (tmpl/while-stmt (core/emit-node (n/while-test node))
                   (core/emit-node (n/while-body node))))