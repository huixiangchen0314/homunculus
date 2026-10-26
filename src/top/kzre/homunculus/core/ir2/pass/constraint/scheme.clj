(ns top.kzre.homunculus.core.ir2.pass.constraint.scheme
  "HM 类型方案 (TScheme) 的定义和操作函数。
   完全基于 IType 协议，不使用 instance?。"
  (:require [clojure.set :as set]
            [top.kzre.homunculus.core.ir2.pass.model :as t]
            [top.kzre.homunculus.core.ir2.pass.protocol :as p]))

;; 原自由变量+原类型变量
(defrecord TScheme [vars type]
  p/IType
  (type-kind [_] :scheme))

(defn tscheme?
  "安全判断 x 是否为 TScheme，x 为 nil 或非类型时返回 false。"
  [x]
  (and
    x
    (satisfies? p/IType x)
    (= :scheme (p/type-kind x))))

;; ── 自由类型变量收集 ──
(defn ftv [ty]
  (case (p/type-kind ty)
    :var #{(:id ty)}
    :fun (set/union (ftv (:arg ty)) (ftv (:ret ty)))
    :app (reduce set/union #{} (map ftv (:args ty)))
    :scheme (ftv (:type ty))
    :hetero-map (reduce set/union #{} (map (fn [[_ v]] (ftv v)) (:entries ty)))
    #{}))

;; ── 泛型化 ──
(defn generalize
  "将类型 ty 在环境 type-bindings 下泛型化为 TScheme。
   type-bindings 是绑定映射（变量名 → IType 或 TScheme）。
   即，当前环境中已经确定的类型保持固定，
   当前环境中仍然未知的类型延迟到调用点确认。"
  [ty type-bindings]
  (let [env-ftv (set (mapcat (fn [[_ v]]
                               (when (satisfies? p/IType v)
                                 (if (= :scheme (p/type-kind v))
                                   (ftv (:type v))
                                   (ftv v))))
                             type-bindings))
        ;; 一些环境变量已经由环境中的其他类型进行约束，我们不能泛化它们
        free-vars (set/difference (ftv ty) env-ftv)
        sorted-vars (sort free-vars)]
    (->TScheme (mapv (fn [id] (t/->TVar id)) sorted-vars) ty)))

;; ── 实例化 ──
(defn instantiate [^TScheme scheme]
  ;; 从原类型变量产生新对类型变量，从而产生新的自由类型
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

(defn instantiate-if-scheme
  "若绑定为 scheme 则实例化，否则原样返回。"
  [binding]
  (if (tscheme? binding)
    (instantiate binding)
    binding))