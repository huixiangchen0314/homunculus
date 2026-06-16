
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


;; ── 容器类型 ──────────────────────────────
(defrecord TContainer [kind element-type shape]
  p/IType
  (type-kind [_] :container))

;; ── 形状记录 ──────────────────────────────
(defrecord FixedLength [size]
  p/ICollectionShape
  (shape-kind [_] :fixed))

(defrecord VariableLength []
  p/ICollectionShape
  (shape-kind [_] :variable))


;; 异构向量
(defrecord THeteroVec [types]  ;; types 为元素类型向量，顺序与元素对应
  p/IType
  (type-kind [_] :hetero-vec))

;; 异构map
(defrecord THeteroMap [entries]   ;; entries 是 ([:key1 type1] [:key2 type2] ...) 的有序向量
  p/IType
  (type-kind [_] :hetero-map))
