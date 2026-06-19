(ns top.kzre.homunculus.backend.hlsl.methods.block
  "HLSL :block 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :block [node context]
  (let [stmts (mapv #(core/emit-node % context) (n/block-exprs node))]
    [:block stmts]))