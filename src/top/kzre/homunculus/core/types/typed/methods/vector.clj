(ns top.kzre.homunculus.core.types.typed.methods.vector
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :vector [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [items (:items node)
          inferred (map #(infer/infer % context) items)
          item-nodes (mapv second inferred)
          ty (t/->TCon :vector)
          new-attrs (assoc (ir2p/attrs node) :type ty)]
      [ty (assoc node :items item-nodes :attrs new-attrs)])))