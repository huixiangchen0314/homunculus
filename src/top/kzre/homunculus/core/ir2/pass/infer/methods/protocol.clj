(ns top.kzre.homunculus.core.ir2.pass.infer.methods.protocol
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :protocol [node env]
  ;; 协议是类型定义，推断类型为协议类型本身
  (let [proto-name (n/protocol-name node)
        proto-type (ty/make-tcon proto-name)]
    (infer/success proto-type node env)))