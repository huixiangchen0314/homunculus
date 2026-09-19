(ns top.kzre.homunculus.core.ir2.pass.check.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :lambda [node expected context]
  (let [body-node (check/check-node (n/lambda-body node) nil context)]
    (n/lambda-with-body node body-node)))