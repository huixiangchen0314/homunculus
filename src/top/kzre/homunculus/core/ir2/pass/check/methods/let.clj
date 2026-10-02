(ns top.kzre.homunculus.core.ir2.pass.check.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :let [node expected env]
  (let [bindings         (n/let-bindings node)
        checked-bindings (mapv (fn [binding]
                                 (let [{:keys [var val]} binding
                                       new-var (check/check-node* var nil env)
                                       new-val (check/check-node* val nil env)]
                                   (assoc binding :var new-var
                                                  :val new-val)))
                               bindings)
        body-node        (check/check-node* (n/let-body node) expected env)]
    (n/make-let checked-bindings body-node
                (n/attrs node)
                (n/node-meta node))))