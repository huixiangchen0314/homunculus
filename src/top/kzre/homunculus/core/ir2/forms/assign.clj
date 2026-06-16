(ns top.kzre.homunculus.core.ir2.forms.assign
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :set! [node env]
  (let [var-node (first (ir2/lower-ast (n1/set-var node) env))
        val-node (first (ir2/lower-ast (n1/set-val node) env))]
    [(n2/make-assign var-node val-node {} (n1/node-meta node) nil)]))