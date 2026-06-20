(ns top.kzre.homunculus.core.types.constraint.gen.methods.vector
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.core.types.protocol :as tp]))

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
        ;; 根据后端配置决定向量类型
        backend (u/backend context)
        support-hetero (when backend (tp/support-hetero-vec backend))
        [vec-type extra-constrs]
        (if support-hetero
          ;; 异构向量：保留所有元素类型
          [(ty/make-hetero-vec item-tys) []]
          ;; 同构向量：所有元素类型必须一致，引入公共元素类型变量
          (let [elem-tv (gen/fresh-tvar)
                elem-constrs (mapv (fn [item-ty] (c/make-cequal elem-tv item-ty)) item-tys)]
            [(ty/make-tvec elem-tv (count items)) elem-constrs]))
        new-node (n/make-vector (vec item-nodes)
                                (n/attrs node)
                                (n/node-meta node)
                                (n/parent node))]
    [vec-type (ty/set-type! new-node vec-type) (into constrs extra-constrs) final-ctx]))