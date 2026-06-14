(ns top.kzre.homunculus.core.types.typed.methods.call
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :call [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [env (:env context)
          [fn-ty fn-node s-fn] (infer/infer (:fn node) context)
          ;_ (when-not (type/fun-type? fn-ty)
          ;    (throw (ex-info "Call target is not a function" {:fn-ty fn-ty})))
          args (:args node)
          [arg-tys arg-nodes s-args]
          (loop [arg-irs args, tys [], nodes [], subst s-fn]
            (if (seq arg-irs)
              (let [env' (infer/apply-subst-to-env env subst)
                    [arg-ty arg-node s-arg] (infer/infer (first arg-irs) (assoc context :env env'))]
                (recur (rest arg-irs) (conj tys arg-ty) (conj nodes arg-node) (merge subst s-arg)))
              [tys nodes subst]))
          s (merge s-fn s-args)
          fn-ty' (u/substitute fn-ty s)
          arg-tys' (mapv #(u/substitute % s) arg-tys)
          ret-tv (t/->TVar (gensym "ret"))
          desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv (reverse arg-tys'))
          s-unify (u/unify fn-ty' desired)
          s-final (merge s s-unify)
          ret-ty (u/substitute ret-tv s-final)
          fn-ty-final (u/substitute fn-ty s-final)          ;; 把实例化的 TScheme 写回节点
          fn-node'    (type/set-type! fn-node fn-ty-final)
          new-node (type/set-type! (assoc node :fn fn-node'  :args (vec arg-nodes)) ret-ty)]
      [ret-ty new-node s-final])))