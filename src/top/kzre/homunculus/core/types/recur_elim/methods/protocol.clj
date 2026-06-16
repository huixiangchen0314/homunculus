
(ns top.kzre.homunculus.core.types.recur-elim.methods.protocol
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :protocol [node]
  ;; ProtocolNode 在 IR2 中没有子节点，直接返回
  node)