(ns top.kzre.homunculus.backend.hlsl.methods.member-access
  "HLSL :member-access 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :member-access [node context]
  (let [target (core/emit-node (n/access-target node) context)
        member (n/access-member node)]
    [:member-access target member]))