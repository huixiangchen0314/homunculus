;; top.kzre.homunculus.core.types.infer.methods.literal.clj
(ns top.kzre.homunculus.core.types.infer.methods.literal
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :literal [node context]
  (if-let [frontend (:frontend context)]
    (let [ty (tp/literal->type frontend (:val node))]
      (infer/success ty (assoc-in node [:attrs :type] ty)))
    (infer/nothing node)))