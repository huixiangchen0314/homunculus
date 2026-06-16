(ns top.kzre.homunculus.core.ir2.forms.map
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :map [node env]
  (let [pairs (n1/map-pairs node)                     ;; 交替的键/值 IR1 节点序列
        kvs   (mapv #(first (ir2/lower-ast % env)) pairs)]
    [(n2/make-map kvs {} (n1/node-meta node) nil)]))