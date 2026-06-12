(ns top.kzre.homunculus.core.ir2.forms.if
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :if [node env]
  (let [kids (ir1p/children node)
        test (first kids)
        then (second kids)
        else (nth kids 2 nil)                 ;; 可能为 nil
        test-node (first (ir2/lower-ast test env))
        then-node (first (ir2/lower-ast then env))
        else-node (when else (first (ir2/lower-ast else env)))
        meta (ir2/ir1-meta node)]
    [(m/->IfNode test-node then-node else-node nil meta  nil)]))