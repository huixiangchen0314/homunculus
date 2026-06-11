(ns top.kzre.homunculus.core.types.check.methods.lambda
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :lambda [node expected context]
  ;; lambda 本身通常不需要转换，但可检查体
  (let [body-node (check/check (:body node) nil context)]
    (assoc node :body body-node)))