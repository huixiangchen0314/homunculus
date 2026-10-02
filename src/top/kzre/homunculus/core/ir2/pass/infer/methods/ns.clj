(ns top.kzre.homunculus.core.ir2.pass.infer.methods.ns
  (:require
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :ns [node env]
  ;; ns 节点无子节点，直接返回 nothing
  (infer/nothing node env))