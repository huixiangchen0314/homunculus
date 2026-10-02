(ns top.kzre.homunculus.core.ir2.pass.check.methods.member-access
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :member-access [node expected env]
  (let [new-target (check/check-node* (n/access-target node) nil env)
        new-args   (mapv #(check/check-node* % nil env) (n/access-args node))]
    (n/make-member-access new-target
                          (n/access-member node)
                          new-args
                          (n/attrs node)
                          (n/node-meta node))))