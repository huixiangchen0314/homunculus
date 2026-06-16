(ns top.kzre.homunculus.core.types.lambda-elim.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :record [node _roots _config _defs]
  ;; 声明节点，无子节点，直接返回
  node)