(ns top.kzre.homunculus.core.types.infer.methods.vector
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]))

(defmethod infer/local-infer :vector [node context]
  (let [items (n/vector-items node)
        ;; 顺序推导每个元素，累积新节点、类型以及上下文
        [item-nodes item-tys final-ctx]
        (reduce (fn [[nodes tys ctx] item]
                  (let [[ty new-item new-ctx] (infer/local-infer item ctx)]
                    [(conj nodes new-item) (conj tys ty) new-ctx]))
                [[] [] context]
                items)
        ;; 根据后端配置决定向量类型
        backend (some-> context :backend)
        support-hetero (when backend (tp/support-hetero-vec backend))
        vec-type (if support-hetero
                   ;; 支持异构向量：保留每个元素的独立类型
                   (t/->THeteroVec item-tys)
                   ;; 不支持异构：构造同构向量，元素类型采用第一个元素类型（或待约束求解）
                   (let [elem-ty (first item-tys)]   ; 若无元素则为 nil
                     (t/->TVec elem-ty (count item-tys))))
        new-node (n/vector-with-items node item-nodes)]
    (if (every? some? item-tys)
      (infer/success vec-type (type/set-type! new-node vec-type) final-ctx)
      (infer/nothing (type/set-type! new-node vec-type) final-ctx))))