(ns top.kzre.homunculus.core.ir2.typed-pass.methods.call
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.unify :as u]))

(defmethod infer/infer :call [node env]
  (let [[fn-ty fn-node] (infer/infer (:fn node) env)
        args (map #(infer/infer % env) (:args node))
        arg-tys (map first args)
        arg-nodes (map second args)
        ret-tv (t/fresh-tvar)
        desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv arg-tys)
        subst (u/unify fn-ty desired)
        ret-ty (u/substitute ret-tv subst)]
    [ret-ty (-> node
                (assoc :fn fn-node :args (vec arg-nodes))
                (assoc-in [:attrs :type] ret-ty))]))