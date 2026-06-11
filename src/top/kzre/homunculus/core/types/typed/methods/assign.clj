(ns top.kzre.homunculus.core.types.typed.methods.assign
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :assign [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [[var-ty var-node] (infer/infer (:var node) context)
          [val-ty val-node] (infer/infer (:val node) context)]
      (u/unify var-ty val-ty)
      (let [ty (t/->TCon :nil)
            new-attrs (assoc (ir2p/attrs node) :type ty)]
        [ty (assoc node :var var-node :val val-node :attrs new-attrs)]))))