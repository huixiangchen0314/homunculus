(ns top.kzre.homunculus.core.ir2.pass.check.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :let [node expected env]
  (let [checked-bindings (mapv #(check/check-node* % nil env)
                               (n/let-bindings node))
        body-node        (check/check-node* (n/let-body node) expected env)]
    (n/make-let checked-bindings body-node
                (n/attrs node)
                (n/node-meta node))))