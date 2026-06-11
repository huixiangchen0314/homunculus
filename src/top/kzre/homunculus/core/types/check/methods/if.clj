(ns top.kzre.homunculus.core.types.check.methods.if
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defmethod check/check :if [node expected context]
  (let [test-node (check/check (:test node) (t/->TCon :bool) context)
        then-node (check/check (:then node) expected context)
        else-node (when (:else node)
                    (check/check (:else node) expected context))]
    (assoc node :test test-node :then then-node :else else-node)))