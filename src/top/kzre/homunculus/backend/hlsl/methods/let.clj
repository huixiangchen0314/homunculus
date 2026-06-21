(ns top.kzre.homunculus.backend.hlsl.methods.let
  "HLSL :let 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]))

(defmethod core/emit-node :let [node context]
  ;; let 在 HLSL 中不是合法表达式，仅允许在函数体内使用（由 define 处理）
  (throw (ex-info "Let expr is only valid as return-value of function."
                  {:node node})))