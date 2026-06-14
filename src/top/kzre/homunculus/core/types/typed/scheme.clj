(ns top.kzre.homunculus.core.types.typed.scheme
  (:require
   [clojure.set :as set]
   [top.kzre.homunculus.core.types.model :as t]
   [top.kzre.homunculus.core.types.typed.unify :as u])
  (:import
    [top.kzre.homunculus.core.types.model TApp TContainer TFun THeteroMap TVar]))

(defrecord TScheme [vars type])

;; ── 自由类型变量收集（完整版） ──
(defn ftv [ty]
  (cond
    (instance? TVar ty) #{(:id ty)}
    (instance? TFun ty) (set/union (ftv (:arg ty)) (ftv (:ret ty)))
    (instance? TContainer ty)
    (let [elem (:element-type ty)]
      (if (vector? elem)
        (reduce set/union #{} (map ftv elem))
        (ftv elem)))
    (instance? THeteroMap ty)
    (reduce set/union #{} (map (fn [[_ val-ty]] (ftv val-ty)) (:entries ty)))
    (instance? TApp ty) (reduce set/union #{} (map ftv (:args ty)))
    :else #{}))

;; ── 泛型化 ──
(defn generalize [ty env]
  (let [env-ftv (set (mapcat (fn [[_ v]]
                               (let [t (if (instance? TScheme v) (:type v) v)]
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
    ;; 直接复用统一模块的 substitute，保证所有类型节点都被替换
    (u/substitute (:type scheme) mapping)))