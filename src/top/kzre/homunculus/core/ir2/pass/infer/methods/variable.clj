(ns top.kzre.homunculus.core.ir2.pass.infer.methods.variable
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as core]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod core/infer-node* :variable [node env]
  (if-let [type (or (t/get-type node)
                    (p/resolve-var-type env (n/var-name node)))]
    (core/success type (t/ensure-type node type) env)
    (core/nothing node env)))