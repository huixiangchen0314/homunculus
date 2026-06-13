(ns top.kzre.homunculus.core.types.typed.methods.map
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :map [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [kvs (:kvs node)
          pairs (partition 2 kvs)
          inferred (mapcat (fn [[k v]]
                             (let [[kt kn] (infer/infer k context)
                                   [vt vn] (infer/infer v context)]
                               [kn vn]))
                           pairs)
          ty (t/->TCon :map)
          new-node (type/set-type! (assoc node :kvs (vec inferred)) ty)]
      [ty new-node {}])))