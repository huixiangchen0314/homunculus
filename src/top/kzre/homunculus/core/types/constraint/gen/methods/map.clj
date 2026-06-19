(ns top.kzre.homunculus.core.types.constraint.gen.methods.map
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :map [node context]
  (let [kvs (n/map-kvs node)
        pairs (n/kv-pairs kvs)
        ;; 顺序处理每一对键值，累积上下文、结果节点和约束
        [results final-ctx]
        (reduce
          (fn [[results ctx] [k v]]
            (let [[k-ty k-node k-constr k-ctx] (gen/cg-node-raw k ctx)
                  [v-ty v-node v-constr v-ctx] (gen/cg-node-raw v k-ctx)]
              [(conj results {:key-ty k-ty :val-ty v-ty
                              :key-node k-node :val-node v-node
                              :constraints (concat k-constr v-constr)})
               v-ctx]))
          [[] context]
          pairs)
        ;; 构造 map 的类型（异构 map）
        entries (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type (t/make-hetero-map entries)
        ;; 重建节点：将新的子节点按顺序展平放入 :kvs
        new-kvs (mapcat (fn [{:keys [key-node val-node]}] [key-node val-node]) results)
        new-node (n/map-with-kvs node new-kvs)
        ;; 合并所有子约束
        all-constr (mapcat :constraints results)]
    ;; 返回四元组：类型、新节点、约束、最终上下文
    [map-type (t/set-type! new-node map-type) all-constr final-ctx]))