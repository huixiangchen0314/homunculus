(ns top.kzre.homunculus.core.ir2.forms.param
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :param [node env]
  ;; IR1 Param 节点 → IR2 Param 节点
  [(n2/make-param (:name node) {} (:meta node)) env])