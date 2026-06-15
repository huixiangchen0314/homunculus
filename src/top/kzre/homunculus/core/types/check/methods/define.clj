(ns top.kzre.homunculus.core.types.check.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check :define [node expected context]
  (let [val-node (check/check (n/define-val node) nil context)]
    (n/define-with-val node val-node)))