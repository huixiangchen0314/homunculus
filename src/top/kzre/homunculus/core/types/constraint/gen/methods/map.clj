;; 文件：top/kzre/homunculus/core/types/constraint/gen/methods/map.clj
(ns top.kzre.homunculus.core.types.constraint.gen.methods.map
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :map [node context]
  (let [kvs (n/map-kvs node)                                    ;; 键值对交替列表
        ;; 将键值对按顺序分组为 [[k v] ...]
        pairs (partition 2 kvs)
        ;; 对每一对中的 key 和 value 分别推导，急切求值
        results (mapv (fn [[k v]]
                        (let [[k-ty k-node k-constr] (gen/cg-node-raw k context)
                              [v-ty v-node v-constr] (gen/cg-node-raw v context)]
                          {:key-ty k-ty :val-ty v-ty
                           :key-node k-node :val-node v-node
                           :constraints (concat k-constr v-constr)}))
                      pairs)
        ;; 收集所有键和值的类型，构造 THeteroMap 的 entries
        entries (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type (ty/make-hetero-map entries)
        ;; 重建节点：将所有新子节点按原始顺序展平放回 :kvs
        new-kvs (mapcat (fn [{:keys [key-node val-node]}] [key-node val-node]) results)
        new-node (n/make-map new-kvs
                             (n/attrs node) (n/node-meta node) (n/parent node))
        ;; 合并所有子约束
        all-constr (mapcat :constraints results)]
    [map-type (ty/set-type! new-node map-type) all-constr]))