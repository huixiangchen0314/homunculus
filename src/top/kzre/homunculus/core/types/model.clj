
(ns top.kzre.homunculus.core.types.model
  (:require [top.kzre.homunculus.core.types.protocol :as p]))

;; ── 基本类型 ──────────────────────────────
(defrecord TVar [id]
  p/IType
  (type-kind [_] :var))

(defrecord TCon [name]
  p/IType
  (type-kind [_] :con))

(defrecord TFun [arg ret]
  p/IType
  (type-kind [_] :fun))

(defrecord TApp [ctor args]   ;; 泛型应用，如 (Vector Int32)
  p/IType
  (type-kind [_] :app))


;; ── 同构向量类型 ──────────────────────────────
(defrecord TVec [element-type size]
  p/IType
  (type-kind [_] :vec))

;; 异构向量
(defrecord THeteroVec [types]  ;; types 为元素类型向量，顺序与元素对应
  p/IType
  (type-kind [_] :hetero-vec))

;; 异构map
(defrecord THeteroMap [entries]   ;; entries 是 ([:key1 type1] [:key2 type2] ...) 的有序向量
  p/IType
  (type-kind [_] :hetero-map))


;; 类型级值, 这个值应当是编译时可知的值.
(defrecord TValue [val]
 p/IType
 (type-kind [_] :value))