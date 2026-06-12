(ns top.kzre.homunculus.core.ir2.forms.assign
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :set! [node env]
  (let [kids (ir1p/children node)
        var-node (first (ir2/lower-ast (first kids) env))
        val-node (first (ir2/lower-ast (second kids) env))
        meta (ir2/ir1-meta node)]
    [(m/->AssignNode var-node val-node nil meta nil)]))