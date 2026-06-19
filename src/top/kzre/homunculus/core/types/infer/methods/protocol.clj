(ns top.kzre.homunculus.core.types.infer.methods.protocol
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod infer/local-infer :protocol [node context]
  ;; 协议也是类型定义，将其名称注册到已知类型集合中
  (let [proto-name (n/protocol-name node)
        proto-type (ty/make-tcon proto-name)
        new-ctx (infer/add-known-type context proto-name)]
    ;; 返回协议类型、原节点（协议体不在此 pass 处理）、更新了已知类型的上下文
    (infer/success proto-type node new-ctx)))