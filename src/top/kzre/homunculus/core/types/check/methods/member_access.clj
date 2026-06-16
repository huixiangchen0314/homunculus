(ns top.kzre.homunculus.core.types.check.methods.member-access
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod check/check-node :member-access [node expected context]
  (let [new-target (check/check-node (n/access-target node) nil context)
        new-args   (mapv #(check/check-node % nil context) (n/access-args node))]
    (n/make-member-access new-target
                          (n/access-member node)
                          new-args
                          (n/node-meta node) (n/parent node))))