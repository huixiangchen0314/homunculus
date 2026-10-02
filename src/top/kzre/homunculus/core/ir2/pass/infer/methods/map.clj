(ns top.kzre.homunculus.core.ir2.pass.infer.methods.map
  (:require
    [top.kzre.homunculus.core.ir2.ast :as m]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.model :as t]
    [top.kzre.homunculus.core.ir2.pass.type :as type]))

(defmethod infer/infer-node* :map [node env]
  (let [pairs (:pairs node)                     ;; Pair 节点向量
        [results final-env]
        (reduce (fn [[results current-env] pair-node]
                  (let [k-node (:key pair-node)
                        v-node (:val pair-node)
                        [k-type k-node' k-env] (infer/infer-node* k-node current-env)
                        [v-type v-node' v-env] (infer/infer-node* v-node k-env)
                        ;; 使用 ir2.model 中的 ->Pair 工厂重建
                        new-pair (m/->Pair k-node' v-node'
                                           (:attrs pair-node)
                                           (:meta pair-node))]
                    [(conj results {:key-type k-type
                                    :val-type v-type
                                    :pair-node new-pair})
                     v-env]))
                [[] env]
                pairs)
        entries   (mapv (fn [{:keys [key-type val-type]}] [key-type val-type]) results)
        map-type  (t/->THeteroMap entries)
        new-pairs (mapv :pair-node results)
        new-node  (m/->Map new-pairs (:attrs node) (:meta node))]
    (if (every? (fn [{:keys [key-type val-type]}] (and key-type val-type)) results)
      (infer/success map-type (type/set-type! new-node map-type) final-env)
      (infer/nothing (type/set-type! new-node map-type) final-env))))