(ns top.kzre.homunculus.core.types.constraint.gen.methods.vector
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :vector [node context]
  (let [items (:items node)
        results (mapv #(gen/cg-node-raw % context) items)   ;; 急切求值
        item-tys   (mapv first results)
        item-nodes (mapv second results)
        child-constraints (mapcat #(nth % 2) results)
        ;; 构造异构向量类型：保留每个元素的类型
        vec-type (t/->THeteroVec item-tys)
        new-node (m/->VectorNode (vec item-nodes)
                                 (:attrs node)
                                 (:meta node)
                                 (:parent node))]
    [vec-type
     (ty/set-type! new-node vec-type)
     child-constraints]))   ;; 无额外统一约束