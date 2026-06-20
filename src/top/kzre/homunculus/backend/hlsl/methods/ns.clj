(ns top.kzre.homunculus.backend.hlsl.methods.ns
  "HLSL :ns 节点发射 —— 将命名空间的 :require 依赖编译为 #include 指令。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.types.namespace :as ns-info]))

(defmethod core/emit-node :ns [node context]
  (let [deps      (ns-info/ns-dependency-syms node)
        exclude   (:exclude-ns context #{})
        path-fn   (:module-naming-fn context)
        real-deps (remove exclude deps)]
    (mapv (fn [dep-sym] [:import (path-fn dep-sym)]) real-deps)))