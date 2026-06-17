(ns top.kzre.homunculus.core.types.constraint.gen.methods.block
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :block [node context]
  (let [exprs (n/block-exprs node)
        ;; 使用 cg-node（四元组）顺序处理子节点，传递上下文
        [results final-ctx]
        (reduce
          (fn [[results ctx] expr]
            (let [[tv new-expr constrs new-ctx] (gen/cg-node expr ctx)]
              [(conj results [tv new-expr constrs])
               (or new-ctx ctx)]))  ;; 若子节点未返回新上下文，则沿用旧的
          [[] context]
          exprs)
        types (mapv first results)
        new-exprs (mapv second results)
        constrs (mapcat #(nth % 2) results)
        last-tv (if (seq types) (last types) (ty/make-tcon :nil))
        new-node (n/make-block new-exprs
                               (n/attrs node)
                               (n/node-meta node)
                               (n/parent node))]
    ;; 返回四元组，将块内累积的上下文传递出去
    [last-tv (ty/set-type! new-node last-tv) constrs final-ctx]))