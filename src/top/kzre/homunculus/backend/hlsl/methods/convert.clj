(ns top.kzre.homunculus.backend.hlsl.methods.convert
  "HLSL :convert 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :convert [node]
  (tmpl/type-cast (core/hlsl-type-str (n/convert-dst-ty node))
                  (core/emit-node (n/convert-expr node))))