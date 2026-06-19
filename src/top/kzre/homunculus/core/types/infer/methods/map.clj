(ns top.kzre.homunculus.core.types.infer.methods.map
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.types.model :as t]))

(defmethod infer/local-infer :map [node context]
  (let [kvs (n/map-kvs node)               ;; 键值对序列，交替存放 key-node, val-node
        pairs (n/kv-pairs kvs)
        ;; 顺序处理每一对，累积新节点和上下文
        [results final-ctx]
        (reduce (fn [[pairs ctx] [k-node v-node]]
                  (let [[k-ty k-node k-ctx] (infer/local-infer k-node ctx)
                        [v-ty v-node v-ctx] (infer/local-infer v-node k-ctx)]
                    [(conj pairs {:key-ty k-ty :val-ty v-ty
                                  :key-node k-node :val-node v-node})
                     v-ctx]))
                [[] context]
                pairs)
        ;; 构造异构 map 类型：entries 为 [[key-type val-type] ...]
        entries (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type (t/->THeteroMap entries)
        ;; 重建节点：将新子节点展平放回 :kvs
        new-kvs  (mapcat (fn [{:keys [key-node val-node]}] [key-node val-node]) results)
        new-node (n/map-with-kvs node new-kvs)]
    (if (every? (fn [{:keys [key-ty val-ty]}] (and key-ty val-ty)) results)
      (infer/success map-type (type/set-type! new-node map-type) final-ctx)
      (infer/nothing (type/set-type! new-node map-type) final-ctx))))