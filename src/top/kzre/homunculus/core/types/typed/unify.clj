(ns top.kzre.homunculus.core.types.typed.unify
  (:require [top.kzre.homunculus.core.types.model :as t])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn occur? [tv type]
  (cond
    (instance? TVar type) (= tv type)
    (instance? TFun type) (or (occur? tv (:arg type))
                              (occur? tv (:ret type)))
    :else false))

(defn substitute [type subst]
  (if (instance? TVar type)
    (if-let [new (get subst type)]
      (substitute new subst)
      type)
    (if (instance? TFun type)
      (t/->TFun (substitute (:arg type) subst)
                (substitute (:ret type) subst))
      type)))

(defn unify [t1 t2]
  (letfn [(go [t1 t2 subst]
            (cond
              (= t1 t2) subst
              (instance? TVar t1)
              (if (occur? t1 t2)
                (throw (ex-info "Occurs check failed" {:var t1 :type t2}))
                (assoc subst t1 t2))
              (instance? TVar t2)
              (go t2 t1 subst)
              (and (instance? TFun t1) (instance? TFun t2))
              (let [sub (go (:arg t1) (:arg t2) subst)]
                (go (substitute (:ret t1) sub)
                    (substitute (:ret t2) sub)
                    sub))
              (and (instance? TCon t1) (instance? TCon t2))
              (if (= (:name t1) (:name t2))
                subst
                (throw (ex-info "Type mismatch" {:t1 t1 :t2 t2})))
              :else (throw (ex-info "Cannot unify" {:t1 t1 :t2 t2}))))]
    (go t1 t2 {})))