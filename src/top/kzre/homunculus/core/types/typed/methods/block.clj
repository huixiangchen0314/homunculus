(ns top.kzre.homunculus.core.types.typed.methods.block
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :block [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [exprs (:exprs node)
          ;; 推导每个表达式并收集替换
          results (map #(infer/infer % context) exprs)
          types (map first results)
          nodes (map second results)
          substs (map #(nth % 2) results)
          last-ty (if (seq types) (last types) (t/->TCon :nil))
          s (reduce merge {} substs)
          new-attrs (assoc (ir2p/attrs node) :type last-ty)]
      [last-ty (assoc node :exprs (vec nodes) :attrs new-attrs) s])))