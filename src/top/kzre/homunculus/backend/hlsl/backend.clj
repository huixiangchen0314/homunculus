(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 后端实现，提供类型转换规则。"
  (:require
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

(defrecord HLSLBackend []
  tp/IBackendInfo
  (type-conversion [_ src-ty dst-ty]
    ;; HLSL 中常见的隐式转换规则，返回代价（或 nil 表示不允许）
    (cond
      ;; int -> float 允许
      (and (ty/con-type? src-ty) (ty/con-type? dst-ty)
           (= (ty/type-sym src-ty) :int)
           (= (ty/type-sym dst-ty) :float)) 1
      ;; float -> half 允许
      (and (ty/con-type? src-ty) (ty/con-type? dst-ty)
           (= (ty/type-sym src-ty) :float)
           (= (ty/type-sym dst-ty) :half)) 1
      ;; int -> half 允许
      (and (ty/con-type? src-ty) (ty/con-type? dst-ty)
           (= (ty/type-sym src-ty) :int)
           (= (ty/type-sym dst-ty) :half)) 1
      ;; 其他情况不允许
      :else nil))
  (support-hetero-vec [_] false))
