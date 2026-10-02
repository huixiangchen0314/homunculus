(ns top.kzre.homunculus.core.ir2.pass.infer.methods.literal
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod infer/infer-node* :literal [node env]
  ;; 利用前端的 literal-type 获取字面量的类型，环境不变
  (if-let [type (p/literal-type env (n/lit-val node))]
    (infer/success type (t/ensure-type node type) env)
    (infer/nothing node env)))