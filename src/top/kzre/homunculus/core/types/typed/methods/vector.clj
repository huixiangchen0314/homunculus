(ns top.kzre.homunculus.core.types.typed.methods.vector
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :vector [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [items (:items node)
          inferred (map #(infer/infer % context) items)
          item-nodes (mapv second inferred)
          ty (t/->TCon :vector)
          new-node (type/set-type! (assoc node :items item-nodes) ty)]
      [ty new-node {}])))