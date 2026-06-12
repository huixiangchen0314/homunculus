(ns top.kzre.homunculus.core.types.typed.methods.map
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :map [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [kvs (:kvs node)
          pairs (partition 2 kvs)
          inferred (mapcat (fn [[k v]]
                             (let [[kt kn] (infer/infer k context)
                                   [vt vn] (infer/infer v context)]
                               [kn vn]))
                           pairs)
          ty (t/->TCon :map)
          new-attrs (assoc (ir2p/attrs node) :type ty)]
      [ty (assoc node :kvs (vec inferred) :attrs new-attrs)])))