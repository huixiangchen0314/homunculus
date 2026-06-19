(ns top.kzre.homunculus.core.types.infer.methods.block
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :block [node context]
  (let [exprs (n/block-exprs node)
        ;; 顺序处理每个表达式，累积上下文和节点，同时记录最后一个表达式的类型
        [new-exprs final-ctx last-ty]
        (reduce (fn [[nodes ctx _] expr]
                  (let [[ty new-expr new-ctx] (infer/local-infer expr ctx)]
                    [(conj nodes new-expr) new-ctx ty]))
                [[] context nil]
                exprs)
        ;; 重建 block 节点
        new-node (n/block-with-exprs node new-exprs)]
    ;; 如果最后一个表达式有类型，则 block 的整体类型为该类型
    (if last-ty
      (infer/success last-ty (type/set-type! new-node last-ty) final-ctx)
      (infer/nothing new-node final-ctx))))