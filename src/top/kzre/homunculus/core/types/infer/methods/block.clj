(ns top.kzre.homunculus.core.types.infer.methods.block
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :block [node context]
  (let [exprs (n/block-exprs node)
        ;; 1. 急切求值，避免惰性重复计算
        results (mapv #(infer/local-infer % context) exprs)
        ;; 2. 直接取最后一个结果的类型（peek 对向量是 O(1)）
        last-ty  (first (peek results))
        ;; 3. 提取所有新节点
        new-exprs (mapv second results)]
    (if last-ty
      (infer/success last-ty
                     (-> node
                        (n/block-with-exprs new-exprs)
                         (type/set-type! last-ty)))
      (infer/nothing (n/block-with-exprs node new-exprs)))))