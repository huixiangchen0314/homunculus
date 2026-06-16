(ns top.kzre.homunculus.core.types.check.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :assign [node expected context]
  (let [var-node (check/check-node (n/assign-var node) nil context)
        val-node (check/check-node (n/assign-val node) (ty/get-type var-node) context)]
    (n/make-assign var-node val-node
                   (n/attrs node) (n/node-meta node) (n/parent node))))