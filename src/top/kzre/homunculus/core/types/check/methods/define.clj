(ns top.kzre.homunculus.core.types.check.methods.define
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :define [node expected context]
  (let [val-node (check/check (:val node) nil context)]
    (assoc node :val val-node)))