(ns top.kzre.homunculus.core.types.constraint.unify
  "独立的类型统一引擎，支持 TVar、TCon、TFun、THeteroMap、TVec、THeteroVec。
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
    :vec (or (occur? tv (ty/vec-element-type ty))
             (when (ty/var-type? (ty/vec-size ty))
               (occur? tv (ty/vec-size ty))))
    :hetero-vec (some #(occur? tv %) (ty/hetero-vec-types ty))
    :hetero-map (some #(occur? tv (second %)) (:entries ty))
    false))

;; ── 类型替换 ──
(defn substitute
  "根据替换映射 subst（键为 TVar，值为 IType）对类型 ty 应用替换。
   返回替换后的新类型。"
  [ty subst]
  (let [kind (p/type-kind ty)]
    (case kind
      :var (if-let [new (get subst ty)]
             (recur new subst)
             ty)
      :fun (t/->TFun (substitute (:arg ty) subst)
                     (substitute (:ret ty) subst))
      :vec (t/->TVec (substitute (ty/vec-element-type ty) subst)
                     (let [sz (ty/vec-size ty)]
                       (if (ty/var-type? sz)
                         (substitute sz subst)
                         sz)))    ; 长度如果是常量整数，保持不变
      :hetero-vec (t/->THeteroVec (mapv #(substitute % subst) (ty/hetero-vec-types ty)))
      :hetero-map (t/->THeteroMap
                    (mapv (fn [[k v]] [k (substitute v subst)])
                          (:entries ty)))
      ;; con, app, container 等其他类型直接返回
      ty)))

;; ── 统一 ──
(defn unify
  "尝试使类型 t1 和 t2 相等，返回一个替换映射 subst 使得 apply(subst, t1) = apply(subst, t2)。
   如果不可能统一，则抛出异常。"
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

                 (and (ty/vec-type? t1) (ty/vec-type? t2))
                 (let [len1 (ty/vec-size t1)
                       len2 (ty/vec-size t2)]
                   (if (= len1 len2)  ; 长度必须严格相等（常量或同一类型变量）
                     (go (ty/vec-element-type t1) (ty/vec-element-type t2) subst)
                     (throw (ex-info "TVec length mismatch" {:len1 len1 :len2 len2}))))

                 (and (ty/hetero-vec? t1) (ty/hetero-vec? t2))
                 (let [types1 (ty/hetero-vec-types t1)
                       types2 (ty/hetero-vec-types t2)]
                   (if (= (count types1) (count types2))
                     (reduce (fn [s [e1 e2]] (go e1 e2 s))
                             subst
                             (map vector types1 types2))
                     (throw (ex-info "HeteroVec size mismatch"
                                     {:size1 (count types1) :size2 (count types2)}))))

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