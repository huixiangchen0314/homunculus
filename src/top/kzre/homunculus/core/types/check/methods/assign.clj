(ns top.kzre.homunculus.core.types.check.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod check/check-node :assign [node expected context]
  (let [var-node (check/check-node (n/assign-var node) nil context)
        val-node (check/check-node (n/assign-val node) (type/get-type var-node) context)]
    (assoc node :var var-node :val val-node)))