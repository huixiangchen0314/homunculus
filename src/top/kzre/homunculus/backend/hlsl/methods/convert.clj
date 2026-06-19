(ns top.kzre.homunculus.backend.hlsl.methods.convert
  "HLSL :convert 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :convert [node context]
  (let [dst-type (core/hlsl-type-str (n/convert-dst-ty node))
        expr     (core/emit-node (n/convert-expr node) context)]
    [:cast dst-type expr]))