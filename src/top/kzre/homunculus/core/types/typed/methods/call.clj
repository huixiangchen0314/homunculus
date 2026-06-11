(ns top.kzre.homunculus.core.types.typed.methods.call
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :call [node context]
  (let [[fn-ty fn-node] (infer/infer (:fn node) context)   ;; 直接访问字段 :fn
        args (:args node)
        inferred-args (map #(infer/infer % context) args)
        arg-tys (map first inferred-args)
        arg-nodes (map second inferred-args)
        ret-tv (t/->TVar (gensym "ret"))
        desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv arg-tys)
        _ (u/unify fn-ty desired)
        subst (u/unify fn-ty desired)
        ret-ty (u/substitute ret-tv subst)
        new-attrs (assoc (ir2p/attrs node) :type ret-ty)
        new-node (-> node
                     (assoc :fn fn-node :args (vec arg-nodes) :attrs new-attrs))]
    [ret-ty new-node]))