(ns top.kzre.homunculus.core.ir2.pass.check.methods.binding
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]
            [top.kzre.homunculus.core.ir2.pass.type :as ty]))


(defmethod check/check-node* :binding [node expected env]
  (let [{:keys [var val]} node
        new-var (check/check-node* var nil env)
        var-type (ty/get-type new-var)
        new-val (check/check-node* val var-type env)]
    (assoc node :var new-var :val new-val)))