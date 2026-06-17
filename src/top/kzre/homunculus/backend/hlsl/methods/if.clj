(ns top.kzre.homunculus.backend.hlsl.methods.if
  "HLSL :if 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod core/emit-node :if [node]
  (let [test (core/emit-node (n/if-test node))
        then (core/emit-node (n/if-then node))]
    (if-let [else (n/if-else node)]
      (tmpl/if-else-stmt test then (core/emit-node else))
      (tmpl/if-stmt test then))))