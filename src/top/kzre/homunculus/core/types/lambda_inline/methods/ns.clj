(ns top.kzre.homunculus.core.types.lambda-inline.methods.ns
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :ns [node _config]
  ;; 无子节点，直接返回
  node)