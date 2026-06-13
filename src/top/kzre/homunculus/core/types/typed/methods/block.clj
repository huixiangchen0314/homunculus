(ns top.kzre.homunculus.core.types.typed.methods.block
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :block [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [exprs (:exprs node)
          results (map #(infer/infer % context) exprs)
          types (map first results)
          nodes (map second results)
          substs (map #(nth % 2) results)
          last-ty (if (seq types) (last types) (t/->TCon :nil))
          s (reduce merge {} substs)
          new-node (type/set-type! (assoc node :exprs (vec nodes)) last-ty)]
      [last-ty new-node s])))