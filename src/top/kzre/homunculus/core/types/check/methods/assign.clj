(ns top.kzre.homunculus.core.types.check.methods.assign
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :assign [node expected context]
  (let [var-node (check/check (:var node) nil context)
        val-node (check/check (:val node) (get-in var-node [:attrs :type]) context)]
    (assoc node :var var-node :val val-node)))