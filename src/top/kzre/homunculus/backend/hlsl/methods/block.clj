(ns top.kzre.homunculus.backend.hlsl.methods.block
  "HLSL :block 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]
            [clojure.string :as str]))

(defmethod core/emit-node :block [node]
  (str/join "\n" (mapv core/emit-node (n/block-exprs node))))