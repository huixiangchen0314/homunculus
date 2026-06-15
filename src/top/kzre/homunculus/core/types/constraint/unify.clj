(ns top.kzre.homunculus.core.types.constraint.unify
  "独立的类型统一引擎，支持 TVar、TCon、TFun、THeteroMap。
   不依赖 typed 模块，完全通过 IType 协议操作。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.type :as ty]))

;; ── 发生检查 ──
(defn occur?
  "检查类型变量 tv 是否出现在类型 ty 中。用于防止在统一时构造无限类型。
   tv 必须是 TVar 实例，ty 是任意的 IType。
   返回 true 表示 tv 存在于 ty 的叶节点中，否则返回 false。"
  [tv ty]
  (case (p/type-kind ty)
    :var (= tv ty)
    :fun (or (occur? tv (:arg ty)) (occur? tv (:ret ty)))
    :hetero-map (some #(occur? tv (second %)) (:entries ty))
    false))

;; ── 类型替换 ──
(defn substitute
  "根据替换映射 subst（键为 TVar，值为 IType）对类型 ty 应用替换。
   返回替换后的新类型。如果 ty 为 TVar 且在 subst 中存在映射，则递归替换；
   对于 TFun，替换其参数和返回类型；对于 THeteroMap，替换每个条目中的值类型；
   其他类型直接返回自身。"
  [ty subst]
  (let [kind (p/type-kind ty)]
    (case kind
      :var (if-let [new (get subst ty)]
             (recur new subst)
             ty)
      :fun (t/->TFun (substitute (:arg ty) subst)
                     (substitute (:ret ty) subst))
      :hetero-map (t/->THeteroMap
                    (mapv (fn [[k v]] [k (substitute v subst)])
                          (:entries ty)))
      ;; con, app, container 等其他类型直接返回
      ty)))

;; ── 统一 ──
(defn unify
  "尝试使类型 t1 和 t2 相等，返回一个替换映射 subst 使得 apply(subst, t1) = apply(subst, t2)。
   如果不可能统一，则抛出异常。
   参数：
     t1, t2 —— 要统一的 IType 实例
     subst  —— 可选的初始替换（默认为 {}）
   返回值：一个从 TVar 到 IType 的 map，表示合并后的替换。
   算法：
     - 如果 t1 == t2，直接返回当前 subst。
     - 如果 t1 是类型变量，且不满足 occurs check，则将它绑定到 t2。
     - 如果 t2 是类型变量，交换参数重试。
     - 如果两者都是函数类型，统一它们的参数类型和返回类型。
     - 如果两者都是构造类型（TCon），比较名称，相同则成功，否则失败。
     - 如果两者都是 THeteroMap，要求大小相同，然后逐字段统一。
     - 其他情况无法统一。"
  ([t1 t2] (unify t1 t2 {}))
  ([t1 t2 subst]
   (letfn [(go [t1 t2 subst]
             (let [t1 (substitute t1 subst)
                   t2 (substitute t2 subst)]
               (cond
                 (= t1 t2) subst
                 (ty/var-type? t1)
                 (if (occur? t1 t2)
                   (throw (ex-info "Occurs check failed" {:var t1 :type t2}))
                   (assoc subst t1 t2))
                 (ty/var-type? t2)
                 (go t2 t1 subst)
                 (and (ty/fun-type? t1) (ty/fun-type? t2))
                 (let [s (go (:arg t1) (:arg t2) subst)]
                   (go (substitute (:ret t1) s) (substitute (:ret t2) s) s))
                 (and (ty/con-type? t1) (ty/con-type? t2))
                 (if (= (:name t1) (:name t2))
                   subst
                   (throw (ex-info "Type mismatch" {:t1 t1 :t2 t2})))
                 (and (= (p/type-kind t1) :hetero-map)
                      (= (p/type-kind t2) :hetero-map))
                 (let [entries1 (:entries t1)
                       entries2 (:entries t2)]
                   (when (not= (count entries1) (count entries2))
                     (throw (ex-info "HeteroMap size mismatch" {})))
                   (reduce (fn [s [e1 e2]]
                             (go (second e1) (second e2) s))
                           subst
                           (map vector entries1 entries2)))
                 :else
                 (throw (ex-info "Cannot unify" {:t1 t1 :t2 t2})))))]
     (go t1 t2 subst))))