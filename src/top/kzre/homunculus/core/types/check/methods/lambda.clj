(ns top.kzre.homunculus.core.types.check.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :lambda [node expected context]
  (let [body-node (check/check-node (n/lambda-body node) nil context)]
    (assoc node :body body-node)))