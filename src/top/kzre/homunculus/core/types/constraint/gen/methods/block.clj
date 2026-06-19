(ns top.kzre.homunculus.core.types.constraint.gen.methods.block
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :block [node context]
  (let [exprs (n/block-exprs node)
        ;; 使用 cg-node（四元组）顺序处理子节点，传递上下文
        [results final-ctx]
        (reduce
          (fn [[results ctx] expr]
            (let [[tv new-expr constrs new-ctx] (gen/cg-node expr ctx)]
              [(conj results [tv new-expr constrs]) new-ctx]))
          [[] context]
          exprs)
        types     (mapv first results)
        new-exprs (mapv second results)
        constrs   (mapcat #(nth % 2) results)
        ;; 若块为空，分配新类型变量（通常不会空）
        block-tv  (if (seq types)
                    (last types)
                    (gen/fresh-tvar))
        new-node  (n/make-block new-exprs
                                (n/attrs node)
                                (n/node-meta node)
                                (n/parent node))]
    ;; 返回四元组：类型、节点、约束、最终上下文
    [block-tv (t/set-type! new-node block-tv) constrs final-ctx]))