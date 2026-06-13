(ns top.kzre.homunculus.core.types.check.methods.assign
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :assign [node expected context]
  (let [var-node (check/check (:var node) nil context)
        val-node (check/check (:val node) (type/get-type var-node) context)]
    (assoc node :var var-node :val val-node)))