(ns top.kzre.homunculus.core.ir2.pass.check.methods.assign
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :assign [node expected env]
  (let [var-node (check/check-node* (n/assign-var node) nil env)
        val-node (check/check-node* (n/assign-val node) (ty/get-type var-node) env)]
    (n/make-assign var-node val-node
                   (n/attrs node)
                   (n/node-meta node))))