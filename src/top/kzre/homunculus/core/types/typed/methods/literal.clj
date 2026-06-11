(ns top.kzre.homunculus.core.types.typed.methods.literal
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :literal [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [frontend (:frontend context)
          ty (or (when frontend (tp/meta->type frontend node))
                 (when frontend (tp/literal->type frontend (:val node)))
                 (t/->TVar (gensym "lit")))
          new-attrs (assoc (ir2p/attrs node) :type ty)]
      [ty (assoc node :attrs new-attrs)])))