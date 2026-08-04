(ns top.kzre.homunculus.core.types.infer.methods.map
  (:require [top.kzre.homunculus.core.ir2.ast :as m]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :map [node context]
  (let [pairs (:pairs node)                     ;; Pair 节点向量
        [results final-ctx]
        (reduce (fn [[results ctx] pair-node]
                  (let [k-node (:key pair-node)
                        v-node (:val pair-node)
                        [k-ty k-node' k-ctx] (infer/local-infer k-node ctx)
                        [v-ty v-node' v-ctx] (infer/local-infer v-node k-ctx)
                        ;; 使用 ir2.model 中的 ->Pair 工厂重建
                        new-pair (m/->Pair k-node' v-node'
                                           (:attrs pair-node)
                                           (:meta pair-node))]
                    [(conj results {:key-ty k-ty :val-ty v-ty :pair-node new-pair})
                     v-ctx]))
                [[] context]
                pairs)
        entries  (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type (t/->THeteroMap entries)
        new-pairs (mapv :pair-node results)
        new-node (m/->Map new-pairs (:attrs node) (:meta node))]
    (if (every? (fn [{:keys [key-ty val-ty]}] (and key-ty val-ty)) results)
      (infer/success map-type (type/set-type! new-node map-type) final-ctx)
      (infer/nothing (type/set-type! new-node map-type) final-ctx))))