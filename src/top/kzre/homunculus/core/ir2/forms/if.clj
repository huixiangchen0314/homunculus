(ns top.kzre.homunculus.core.ir2.forms.if
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :if [node env]
  (let [test      (n1/if-test node)
        then      (n1/if-then node)
        else      (n1/if-else node)
        test-node (first (ir2/lower-ast test env))
        then-node (first (ir2/lower-ast then env))
        else-node (when else (first (ir2/lower-ast else env)))]
    [(n2/make-if test-node then-node else-node {} (n1/node-meta node) nil)]))