(ns top.kzre.homunculus.core.types.check.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :let [node expected context]
  (let [bindings (n/let-bindings node)
        checked-bindings (mapv (fn [bnd]
                                 (let [{:keys [var val]} bnd
                                       new-var (check/check-node var nil context)
                                       new-val (check/check-node val nil context)]
                                   (assoc bnd :var new-var
                                              :val new-val)))
                               bindings)
        body-node (check/check-node (n/let-body node) expected context)]
    (n/make-let checked-bindings body-node
                (n/attrs node) (n/node-meta node))))