(ns top.kzre.homunculus.core.types.constraint.gen.methods.block
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :block [node context]
  (let [exprs (n/block-exprs node)
        results (map #(gen/cg-node-raw % context) exprs)
        types (map first results)
        new-exprs (mapv second results)
        constrs (mapcat #(nth % 2) results)
        last-tv (if (seq types) (last types) (ty/make-tcon :nil))
        new-node (n/make-block new-exprs
                               (n/attrs node)
                               (n/node-meta node)
                               (n/parent node))]
    [last-tv (ty/set-type! new-node last-tv) constrs]))