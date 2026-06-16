(ns top.kzre.homunculus.core.ir2.forms.block
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :do [node env]
  (let [expr-nodes (mapv #(first (ir2/lower-ast % env)) (n1/do-exprs node))]
    [(n2/make-block expr-nodes {} (n1/node-meta node) nil)]))