(ns top.kzre.homunculus.backend.hlsl.methods.block
  "HLSL :block 节点发射。返回无标签向量，中间语句包裹 :expr-stmt，
   最后一个表达式保持原样（不加分号）。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(def ^:private block-level-tags
  #{:if :while :expr-stmt :comment :raw})

(defn- wrap-as-stmt [node]
  (if (or (not (vector? node)) (contains? block-level-tags (first node)))
    node
    [:expr-stmt node]))

(defmethod core/emit-node :block [node context]
  (let [exprs (n/block-exprs node)
        inits (butlast exprs)
        last  (last exprs)
        stmt-asts (mapv #(core/emit-node % context) inits)
        last-ast  (core/emit-node last context)]
    (if (seq stmt-asts)
      (vec (concat
             (mapcat (fn [a]
                       (if (and (vector? a) (not (keyword? (first a))))
                         a
                         [(wrap-as-stmt a)]))
                     stmt-asts)
             ;; ★ 最后一个表达式原样保留（不包裹 :expr-stmt）
             (if (and (vector? last-ast) (not (keyword? (first last-ast))))
               last-ast
               [last-ast])))
      ;; 只有一个表达式时直接返回
      last-ast)))