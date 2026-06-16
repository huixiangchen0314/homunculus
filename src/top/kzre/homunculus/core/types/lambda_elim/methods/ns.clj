(ns top.kzre.homunculus.core.types.lambda-elim.methods.ns
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :ns [node _roots _config _defs]
  ;; 无子节点，直接返回
  node)