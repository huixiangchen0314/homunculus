(ns top.kzre.homunculus.core.types.constraint.gen.methods.vector
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :vector [node context]
  (let [items (n/vector-items node)
        ;; 顺序处理每个元素，累积上下文、新节点、类型、约束
        [item-nodes item-tys constrs final-ctx]
        (reduce
          (fn [[nodes tys constrs ctx] item]
            (let [[tv new-item cc new-ctx] (gen/cg-node-raw item ctx)]
              [(conj nodes new-item) (conj tys tv) (into constrs cc) new-ctx]))
          [[] [] [] context]
          items)
        ;; 构造异构向量类型
        vec-type (ty/make-hetero-vec item-tys)
        new-node (n/make-vector (vec item-nodes)
                                (n/attrs node)
                                (n/node-meta node)
                                (n/parent node))]
    [vec-type (ty/set-type! new-node vec-type) constrs final-ctx]))