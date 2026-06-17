(ns top.kzre.homunculus.backend.hlsl.methods.call
  "HLSL :call 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]
            [clojure.string :as str]))

(defmethod core/emit-node :call [node]
  (let [fn-name (core/emit-node (n/call-fn node))
        args    (mapv core/emit-node (n/call-args node))]
    (tmpl/call fn-name (str/join ", " args))))