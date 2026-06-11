(ns top.kzre.homunculus.core.ir2.typed-pass.methods.block
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]))

(defmethod infer/infer :block [node env]
  (let [pairs (map #(infer/infer % env) (:exprs node))
        types (map first pairs)
        nodes (map second pairs)
        last-ty (if (seq types) (last types) (t/->TCon :nil))]
    [last-ty (assoc node :exprs (vec nodes)
                         :attrs (assoc (:attrs node) :type last-ty))]))