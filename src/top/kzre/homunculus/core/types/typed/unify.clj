(ns top.kzre.homunculus.core.types.typed.unify
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as p])
  (:import [top.kzre.homunculus.core.types.model TVar TFun TContainer]))

(defn occur? [tv type]
  (case (p/type-kind type)
    :var (= tv type)
    :fun (or (occur? tv (:arg type))
             (occur? tv (:ret type)))
    :container (let [elem-ty (:element-type type)]
                 (if (vector? elem-ty)
                   (or (occur? tv (first elem-ty)) (occur? tv (second elem-ty)))
                   (occur? tv elem-ty)))
    false))

(defn substitute [type subst]
  (if (satisfies? p/IType type)
    (case (p/type-kind type)
      :var (if-let [new (get subst type)]
             (substitute new subst)
             type)
      :fun (t/->TFun (substitute (:arg type) subst)
                     (substitute (:ret type) subst))
      :container (let [elem-ty (:element-type type)
                       shape (:shape type)]
                   (if (vector? elem-ty)
                     (t/->TContainer (:kind type) [(substitute (first elem-ty) subst) (substitute (second elem-ty) subst)] shape)
                     (t/->TContainer (:kind type) (substitute elem-ty subst) shape)))
      type)  ;; 其他 IType 直接返回
    type))  ;; 非 IType（如 TScheme）直接返回

(defn unify [t1 t2]
  (letfn [(go [t1 t2 subst]
            (cond
              (= t1 t2) subst
              (= :var (p/type-kind t1))
              (if (occur? t1 t2)
                (throw (ex-info "Occurs check failed" {:var t1 :type t2}))
                (assoc subst t1 t2))
              (= :var (p/type-kind t2))
              (go t2 t1 subst)
              (= :fun (p/type-kind t1) (p/type-kind t2))
              (let [sub (go (:arg t1) (:arg t2) subst)]
                (go (substitute (:ret t1) sub)
                    (substitute (:ret t2) sub)
                    sub))
              (and (= :con (p/type-kind t1)) (= :con (p/type-kind t2)))
              (if (= (:name t1) (:name t2))
                subst
                (throw (ex-info "Type mismatch" {:t1 t1 :t2 t2})))
              (and (= :container (p/type-kind t1)) (= :container (p/type-kind t2)))
              (let [sub1 (go (:element-type t1) (:element-type t2) subst)]
                (if (or (= (type (:shape t1)) (type (:shape t2)))
                        (= :variable (p/shape-kind (:shape t1)))
                        (= :variable (p/shape-kind (:shape t2))))
                  sub1
                  (throw (ex-info "Shape mismatch" {:t1 t1 :t2 t2}))))
              :else (throw (ex-info "Cannot unify" {:t1 t1 :t2 t2}))))]
    (go t1 t2 {})))