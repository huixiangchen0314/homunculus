(ns top.kzre.homunculus.core.types.infer.methods.ns
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :ns [node context]
  ;; ns 节点无子节点，直接返回 nothing
  (infer/nothing node))