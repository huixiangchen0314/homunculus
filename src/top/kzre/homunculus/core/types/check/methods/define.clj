(ns top.kzre.homunculus.core.types.check.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :define [node expected context]
  (let [val-node (check/check-node (n/define-val node) nil context)]
    (n/define-with-val node val-node)))