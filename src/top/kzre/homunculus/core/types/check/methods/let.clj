(ns top.kzre.homunculus.core.types.check.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check :let [node expected context]
  (let [bindings (n/let-bindings node)
        checked-bindings (mapv (fn [[var val]]
                                 [(check/check var nil context)
                                  (check/check val nil context)])
                               bindings)
        body-node (check/check (n/let-body node) expected context)]
    (n/let-with-children node checked-bindings body-node)))