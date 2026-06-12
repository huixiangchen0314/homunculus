(ns top.kzre.homunculus.core.types.check.methods.while
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :while [node expected context]
  (let [test-node (check/check (:test node) (t/->TCon :bool) context)
        body-node (check/check (:body node) nil context)]
    (assoc node :test test-node :body body-node)))