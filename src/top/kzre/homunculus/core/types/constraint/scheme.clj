(ns top.kzre.homunculus.core.types.constraint.scheme
  "HM 类型方案 (TScheme) 的定义和操作函数。
   完全基于 IType 协议，不使用 instance?。"
  (:require [clojure.set :as set]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as p]))

;; ── 记录定义 ──
(defrecord TScheme [vars type]
  p/IType
  (type-kind [_] :scheme))

;; ── 判断函数 ──
(defn tscheme?
  "基于协议判断 x 是否为 TScheme。"
  [x]
  (= :scheme (p/type-kind x)))

;; ── 自由类型变量收集 ──
(defn ftv [ty]
  (case (p/type-kind ty)
    :var #{(:id ty)}
    :fun (set/union (ftv (:arg ty)) (ftv (:ret ty)))
    :container
    (let [elem (:element-type ty)]
      (if (vector? elem)
        (reduce set/union #{} (map ftv elem))
        (ftv elem)))
    :app (reduce set/union #{} (map ftv (:args ty)))
    :scheme (ftv (:type ty))
    :hetero-map (reduce set/union #{} (map (fn [[_ v]] (ftv v)) (:entries ty)))
    #{}))

;; ── 泛型化 ──
(defn generalize [ty env]
  (let [env-ftv (set (mapcat (fn [[_ v]]
                               (let [t (if (= :scheme (p/type-kind v))
                                         (:type v)
                                         v)]
                                 (ftv t)))
                             env))
        free-vars (set/difference (ftv ty) env-ftv)
        sorted-vars (sort free-vars)]
    (->TScheme (mapv (fn [id] (t/->TVar id)) sorted-vars) ty)))

;; ── 实例化 ──
(defn instantiate [^TScheme scheme]
  (let [mapping (reduce (fn [m v] (assoc m (:id v) (t/->TVar (gensym "i"))))
                        {}
                        (:vars scheme))]
    (letfn [(subst-fn [ty]
              (case (p/type-kind ty)
                :var (or (get mapping (:id ty)) ty)
                :fun (t/->TFun (subst-fn (:arg ty)) (subst-fn (:ret ty)))
                :hetero-map (t/->THeteroMap (mapv (fn [[k v]] [k (subst-fn v)]) (:entries ty)))
                ty))]
      (subst-fn (:type scheme)))))