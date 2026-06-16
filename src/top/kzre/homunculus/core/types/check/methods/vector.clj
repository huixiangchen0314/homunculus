(ns top.kzre.homunculus.core.types.check.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :vector [node expected context]
  (let [items (n/vector-items node)]
    (cond
      ;; 异构向量期望：按位置逐元素检查
      (and expected (ty/hetero-vec? expected))
      (let [exp-types (ty/hetero-vec-types expected)]
        (if (= (count items) (count exp-types))
          (let [checked-items (mapv (fn [item exp-ty]
                                      (check/check-node item exp-ty context))
                                    items exp-types)]
            (n/make-vector checked-items (n/attrs node) (n/node-meta node) (n/parent node)))
          (throw (ex-info "Vector length mismatch"
                          {:expected (count exp-types) :actual (count items)}))))

      ;; 统一元素类型向量期望 (TContainer :vector)
      (and expected (ty/container-type? expected) (= (ty/container-kind expected) :vector))
      (let [elem-ty (ty/container-element-type expected)
            checked-items (mapv #(check/check-node % elem-ty context) items)]
        (n/make-vector checked-items (n/attrs node) (n/node-meta node) (n/parent node)))

      ;; 无期望或未知类型
      :else
      (let [checked-items (mapv #(check/check-node % nil context) items)]
        (n/make-vector checked-items (n/attrs node) (n/node-meta node) (n/parent node))))))