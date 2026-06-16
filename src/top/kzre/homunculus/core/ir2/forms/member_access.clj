(ns top.kzre.homunculus.core.ir2.forms.member-access
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :member-access [node env]
  (let [target   (n1/access-target node)
        accessor (n1/access-member node)
        args     (n1/access-args node)
        new-target (first (ir2/lower-ast target env))
        new-args   (mapv #(first (ir2/lower-ast % env)) args)]
    [(n2/make-member-access new-target accessor new-args
                            (n1/node-meta node)
                            nil)]))