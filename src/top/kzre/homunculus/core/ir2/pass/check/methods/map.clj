(ns top.kzre.homunculus.core.ir2.pass.check.methods.map
  (:require
    [top.kzre.homunculus.core.ir2.ast :as m]               ; Pair, Map 记录与工厂
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :map [node expected env]
  (let [pairs      (:pairs node)              ;; Pair 向量
        pair-count (count pairs)]
    (if (and expected (ty/hetero-map? expected))
      (let [expected-entries (ty/hetero-map-entries expected)]
        (if (= pair-count (count expected-entries))
          (let [checked-pairs (mapv (fn [pair-node [key-type val-type]]
                                      (let [k-node (check/check-node* (:key pair-node) key-type env)
                                            v-node (check/check-node* (:val pair-node) val-type env)]
                                        (m/->Pair k-node v-node
                                                  (:attrs pair-node)
                                                  (:meta pair-node))))
                                    pairs expected-entries)]
            (m/->Map checked-pairs (:attrs node) (:meta node)))
          (throw (ex-info "Map entry count mismatch"
                          {:expected (count expected-entries) :actual pair-count}))))
      ;; 无期望类型或非 hetero-map：逐一检查键值，期望类型为 nil
      (let [checked-pairs (mapv (fn [pair-node]
                                  (let [k-node (check/check-node* (:key pair-node) nil env)
                                        v-node (check/check-node* (:val pair-node) nil env)]
                                    (m/->Pair k-node v-node
                                              (:attrs pair-node)
                                              (:meta pair-node))))
                                pairs)]
        (m/->Map checked-pairs (:attrs node) (:meta node))))))