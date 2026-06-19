(ns top.kzre.homunculus.core.types.infer.methods.vector
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.types.model :as t]))

(defmethod infer/local-infer :vector [node context]
  (let [items (n/vector-items node)
        ;; 顺序推导每个元素，累积新节点、类型以及上下文
        [item-nodes item-tys final-ctx]
        (reduce (fn [[nodes tys ctx] item]
                  (let [[ty new-item new-ctx] (infer/local-infer item ctx)]
                    [(conj nodes new-item) (conj tys ty) new-ctx]))
                [[] [] context]
                items)
        ;; 构造异构向量类型：保留每个元素的独立类型
        vec-type (t/->THeteroVec item-tys)
        new-node (n/vector-with-items node item-nodes)]
    (if (every? some? item-tys)
      (infer/success vec-type (type/set-type! new-node vec-type) final-ctx)
      (infer/nothing (type/set-type! new-node vec-type) final-ctx))))