;; top.kzre.homunculus.core.types.infer.methods.literal.clj
(ns top.kzre.homunculus.core.types.infer.methods.literal
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod infer/local-infer :literal [node context]
  ;; 利用前端的 literal->type 获取字面量的类型，上下文不变
  (if-let [frontend (infer/frontend context)]
    (let [ty (tp/literal->type frontend (n/lit-val node))]
      (infer/success ty (t/ensure-type node ty) context))
    (infer/nothing node context)))