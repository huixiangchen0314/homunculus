
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

(defrecord MapShape []
  p/ICollectionShape
  (shape-kind [_] :map))

;; 异构map
(defrecord THeteroMap [entries]   ;; entries 是 ([:key1 type1] [:key2 type2] ...) 的有序向量
  p/IType
  (type-kind [_] :hetero-map))

(defrecord SetShape []
  p/ICollectionShape
  (shape-kind [_] :set))


;; ── 常用的前端信息实现（Clojure 默认）──
(deftype ClojureFrontend []
  p/IFrontendInfo
  (frontend-types [_] [:vector :map :set :keyword :symbol :int64 :float64 :bool :string])
  (literal->type [_ val]
    (cond
      (instance? java.lang.Long    val) (->TCon :int64)
      (instance? java.lang.Integer val) (->TCon :int32)
      (instance? java.lang.Double  val) (->TCon :float64)
      (instance? java.lang.Float   val) (->TCon :float32)
      (instance? java.lang.Boolean val) (->TCon :bool)
      (instance? java.lang.String  val) (->TCon :string)
      (keyword? val)                    (->TCon :keyword)
      (nil? val)                        (->TCon :nil)
      :else (throw (ex-info "Unsupported literal" {:val val}))))
  (meta->type [_ node]
    (when-let [tag (get-in node [:meta :tag])]
      (if (keyword? tag)
        (->TCon tag)
        (->TCon (keyword (name tag))))))
  (infer-collection-type [this form]
    (cond
      (vector? form)
      (let [elements (map #(p/literal->type this %) form)
            unified (if (seq elements)
                      (first elements)   ;; 简化：假设元素类型相同
                      (->TVar (gensym "t")))
            shape (if (every? (fn [e] (or (number? e) (string? e) (keyword? e))) form)
                    (->FixedLength (count form))
                    (->VariableLength))]
        (->TContainer :vector unified shape))
      (map? form)
      (let [kvs (apply concat form)
            key-ty (if (seq kvs) (p/literal->type this (first kvs)) (->TVar (gensym "tk")))
            val-ty (if (seq kvs) (p/literal->type this (second kvs)) (->TVar (gensym "tv")))
            shape (->MapShape)]
        (->TContainer :map [key-ty val-ty] shape))
      :else (throw (ex-info "Unsupported collection" {:form form}))))
  (collection-type-ctor [this kind element-type shape]
    (->TContainer kind element-type shape)))