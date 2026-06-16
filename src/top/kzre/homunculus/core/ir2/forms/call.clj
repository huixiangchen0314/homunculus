(ns top.kzre.homunculus.core.ir2.forms.call
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :call [node env]
  (let [fn-node (first (ir2/lower-ast (n1/call-op node) env))
        args    (mapv #(first (ir2/lower-ast % env)) (n1/call-args node))]
    [(n2/make-call fn-node args {} (n1/node-meta node) nil)]))