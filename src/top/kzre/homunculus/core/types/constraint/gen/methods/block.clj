(ns top.kzre.homunculus.core.types.constraint.gen.methods.block
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :block [node context]
  (let [exprs (:exprs node)
        results (map #(gen/cg-node-raw % context) exprs)
        types (map first results)
        new-exprs (mapv second results)
        constrs (mapcat #(nth % 2) results)
        last-tv (if (seq types) (last types) (t/->TCon :nil))
        new-node (m/->BlockNode new-exprs (:attrs node) (:meta node) (:parent node))]
    [last-tv (ty/set-type! new-node last-tv) constrs]))