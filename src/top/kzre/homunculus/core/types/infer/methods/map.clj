(ns top.kzre.homunculus.core.types.infer.methods.map
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.types.model :as t]))

(defmethod infer/local-infer :map [node context]
  (let [kvs (n/map-kvs node)               ;; 键值对序列，交替存放 key-node, val-node
        ;; 将 kvs 按对分组
        pairs (partition 2 kvs)
        ;; 逐对推导
        results (mapv (fn [[k-node v-node]]
                        (let [[k-ty k-new] (infer/local-infer k-node context)
                              [v-ty v-new] (infer/local-infer v-node context)]
                          {:key-ty k-ty :val-ty v-ty
                           :key-node k-new :val-node v-new}))
                      pairs)
        ;; 构造异构 map 类型：entries 为 [[key-type val-type] ...]
        entries (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type (t/->THeteroMap entries)
        ;; 重建节点：需要将新子节点展平放回 :kvs
        new-kvs  (mapcat (fn [{:keys [key-node val-node]}] [key-node val-node]) results)
        new-node (n/map-with-kvs node new-kvs)]
    (if (every? (fn [{:keys [key-ty val-ty]}] (and key-ty val-ty)) results)
      (infer/success map-type (type/set-type! new-node map-type))
      (infer/nothing (type/set-type! new-node map-type)))))