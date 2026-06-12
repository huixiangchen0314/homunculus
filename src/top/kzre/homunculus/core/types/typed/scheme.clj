(ns top.kzre.homunculus.core.types.typed.scheme
  (:require [clojure.set :as set]
            [top.kzre.homunculus.core.types.model :as t])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defrecord TScheme [vars type])

(defn ftv [ty]
  (cond
    (instance? TVar ty) #{(:id ty)}
    (instance? TFun ty) (set/union (ftv (:arg ty)) (ftv (:ret ty)))
    :else #{}))

(defn generalize [ty env]
  (let [env-ftv (set (mapcat (fn [[_ v]]
                               (ftv (if (instance? TScheme v) (:type v) v)))
                             env))
        free-vars (set/difference (ftv ty) env-ftv)
        sorted-vars (sort free-vars)]
    (->TScheme (mapv (fn [id] (t/->TVar id)) sorted-vars) ty)))

(defn instantiate [^TScheme scheme]
  (let [mapping (reduce (fn [m v] (assoc m (:id v) (t/->TVar (gensym "i"))))
                        {}
                        (:vars scheme))
        subst-fn (fn self [ty]
                   (cond
                     (instance? TVar ty) (or (get mapping (:id ty)) ty)
                     (instance? TFun ty) (t/->TFun (self (:arg ty)) (self (:ret ty)))
                     :else ty))]
    (subst-fn (:type scheme))))