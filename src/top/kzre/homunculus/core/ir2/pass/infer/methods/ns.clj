(ns top.kzre.homunculus.core.ir2.pass.infer.methods.ns
  (:require [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/local-infer :ns [node context]
  ;; ns 节点无子节点，直接返回 nothing
  (infer/nothing node context))