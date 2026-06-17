(ns top.kzre.homunculus.backend.hlsl.methods.lambda
  "HLSL :lambda 节点发射。HLSL 不支持闭包，若此处出现 lambda 说明闭包消除未完成，直接报错。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]))

(defmethod core/emit-node :lambda [node]
  (throw (ex-info "Lambda not supported in HLSL (closure elimination may have failed)" {:node node})))