(ns top.kzre.homunculus.core.ir2.forms.block
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :do [node env]
  (let [expr-nodes (mapv #(first (ir2/lower-ast % env)) (ir1p/children node))
        meta (ir2/ir1-meta node)]
    [(m/->BlockNode expr-nodes nil meta expr-nodes nil)]))