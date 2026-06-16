(ns top.kzre.homunculus.core.ir2.forms.vector
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :vector [node env]
  (let [items (mapv #(first (ir2/lower-ast % env)) (n1/vec-items node))]
    [(n2/make-vector items {} (n1/node-meta node) nil)]))