;; top.kzre.homunculus.core.types.infer.methods.literal.clj
(ns top.kzre.homunculus.core.types.infer.methods.literal
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod infer/local-infer :literal [node context]
  (if-let [frontend (infer/frontend context)]
    (let [ty (tp/literal->type frontend (n/lit-val node))]
      (infer/success ty (t/ensure-type node ty)))
    (infer/nothing node)))