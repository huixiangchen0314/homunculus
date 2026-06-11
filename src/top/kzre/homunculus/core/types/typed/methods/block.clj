(ns top.kzre.homunculus.core.types.typed.methods.block
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :block [node context]
  (let [exprs (:exprs node)
        inferred (map #(infer/infer % context) exprs)
        types (map first inferred)
        nodes (map second inferred)
        last-ty (if (seq types) (last types) (t/->TCon :nil))
        new-attrs (assoc (ir2p/attrs node) :type last-ty)
        new-node (assoc node :exprs (vec nodes) :attrs new-attrs)]
    [last-ty new-node]))