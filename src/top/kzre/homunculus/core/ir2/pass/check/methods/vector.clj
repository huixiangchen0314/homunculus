(ns top.kzre.homunculus.core.ir2.pass.check.methods.vector
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :vector [node expected env]
  (let [items (n/vector-items node)]
    (cond
      ;; 异构向量期望：按位置逐元素检查
      (and expected (ty/hetero-vec? expected))
      (let [expected-types (ty/hetero-vec-types expected)]
        (if (= (count items) (count expected-types))
          (let [checked-items (mapv (fn [item expected-type]
                                      (check/check-node* item expected-type env))
                                    items expected-types)]
            (n/make-vector checked-items (n/attrs node) (n/node-meta node)))
          (throw (ex-info "Vector length mismatch"
                          {:expected (count expected-types)
                           :actual   (count items)}))))

      ;; 同构向量期望 (TVec)
      (and expected (ty/vec-type? expected))
      (let [elem-type     (ty/vec-element-type expected)
            expected-size (ty/vec-size expected)
            size-value    (when (ty/type-value? expected-size)
                            (ty/value-val expected-size))
            actual-len    (count items)]
        ;; 期望长度是整数常量时才比对长度
        (when (and (integer? size-value)
                   (not= size-value actual-len))
          (throw (ex-info "Vector length mismatch"
                          {:expected size-value :actual actual-len})))
        (let [checked-items (mapv #(check/check-node* % elem-type env) items)]
          (n/make-vector checked-items (n/attrs node) (n/node-meta node))))

      ;; 无期望或未知期望：逐元素无类型检查
      :else
      (let [checked-items (mapv #(check/check-node* % nil env) items)]
        (n/make-vector checked-items (n/attrs node) (n/node-meta node))))))