(ns top.kzre.homunculus.core.types.check.methods.map
  (:require [top.kzre.homunculus.core.ir2.ast :as m]          ; Pair, Map 记录与工厂
            [top.kzre.homunculus.core.ir2.node :as n]            ; 访问器 (attrs, node-meta)
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :map [node expected context]
  (let [pairs (:pairs node)                                   ;; Pair 向量
        pair-count (count pairs)]
    (if (and expected (ty/hetero-map? expected))
      (let [exp-entries (ty/hetero-map-entries expected)]
        (if (= pair-count (count exp-entries))
          (let [checked-pairs (mapv (fn [pair-node [k-ty v-ty]]
                                      (let [k-node (check/check-node (:key pair-node) k-ty context)
                                            v-node (check/check-node (:val pair-node) v-ty context)]
                                        (m/->Pair k-node v-node
                                                  (:attrs pair-node)
                                                  (:meta pair-node))))
                                    pairs exp-entries)]
            (m/->Map checked-pairs (:attrs node) (:meta node)))
          (throw (ex-info "Map entry count mismatch"
                          {:expected (count exp-entries) :actual pair-count}))))
      ;; 无期望类型或非 hetero-map：逐一检查键值，期望类型为 nil
      (let [checked-pairs (mapv (fn [pair-node]
                                  (let [k-node (check/check-node (:key pair-node) nil context)
                                        v-node (check/check-node (:val pair-node) nil context)]
                                    (m/->Pair k-node v-node
                                              (:attrs pair-node)
                                              (:meta pair-node))))
                                pairs)]
        (m/->Map checked-pairs (:attrs node) (:meta node))))))