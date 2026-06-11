(ns top.kzre.homunculus.core.types.check.methods.let
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :let [node expected context]
  (let [bindings (:bindings node)  ;; [[var val] ...]
        checked-bindings (mapv (fn [[var val]]
                                 [(check/check var nil context)
                                  (check/check val nil context)])
                               bindings)
        body-node (check/check (:body node) expected context)]
    (assoc node :bindings checked-bindings :body body-node)))