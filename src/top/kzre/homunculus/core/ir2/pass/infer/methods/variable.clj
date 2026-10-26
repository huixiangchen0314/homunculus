(ns top.kzre.homunculus.core.ir2.pass.infer.methods.variable
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as core]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod core/infer-node* :variable [node env]
  (if-let [type (or (ty/get-type node)
                    (p/resolve-var-type env (n/var-name node)))]
    (if (ty/concrete? type)
      (core/success type (ty/ensure-type node type) env)
      (core/nothing node env))
    (core/nothing node env)))