(ns top.kzre.homunculus.core.types.check.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check])
  (:import (top.kzre.homunculus.core.types.model TContainer THeteroVec)))

(defmethod check/check :vector [node expected context]
  (let [items (n/vector-items node)]
    (cond
      ;; 异构向量期望：按位置逐元素检查
      (and expected (instance? THeteroVec expected))
      (let [exp-types (:types expected)]
        (if (= (count items) (count exp-types))
          (let [checked-items (mapv (fn [item exp-ty]
                                      (check/check item exp-ty context))
                                    items exp-types)]
            (n/vector-with-items node checked-items))
          (throw (ex-info "Vector length mismatch"
                          {:expected (count exp-types) :actual (count items)}))))

      ;; 统一元素类型向量期望 (TContainer :vector)
      (and expected (instance? TContainer expected) (= (:kind expected) :vector))
      (let [elem-ty (:element-type expected)
            checked-items (mapv #(check/check % elem-ty context) items)]
        (n/vector-with-items node checked-items))

      ;; 无期望或未知类型
      :else
      (let [checked-items (mapv #(check/check % nil context) items)]
        (n/vector-with-items node checked-items)))))