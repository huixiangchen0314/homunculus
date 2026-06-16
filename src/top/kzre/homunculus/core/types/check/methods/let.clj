(ns top.kzre.homunculus.core.types.check.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :let [node expected context]
  (let [bindings (n/let-bindings node)
        checked-bindings (mapv (fn [[var val]]
                                 [(check/check-node var nil context)
                                  (check/check-node val nil context)])
                               bindings)
        body-node (check/check-node (n/let-body node) expected context)]
    (n/let-with-children node checked-bindings body-node)))