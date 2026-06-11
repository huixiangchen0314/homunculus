;; top.kzre.homunculus.core.types.infer.methods.block.clj
(ns top.kzre.homunculus.core.types.infer.methods.block
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :block [node context]
  (let [exprs (:exprs node)
        results (map #(infer/local-infer % context) exprs)
        types (map first results)
        nodes (map second results)
        last-ty (last types)]
    (if last-ty
      (infer/success last-ty
                     (assoc node :exprs (vec nodes) :attrs (assoc (:attrs node) :type last-ty)))
      (infer/nothing (assoc node :exprs (vec nodes))))))