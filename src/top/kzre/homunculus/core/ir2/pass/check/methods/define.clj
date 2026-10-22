(ns top.kzre.homunculus.core.ir2.pass.check.methods.define
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :define [node expected env]
  (if-let [val-node (n/define-val node)]
    ;; 用自己的类型去期待值的类型
    (let [t (ty/get-type node)
          val-node (check/check-node* val-node t env)]
      (n/define-with-val node val-node))
    node))