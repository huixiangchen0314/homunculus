(ns top.kzre.homunculus.core.types.infer.methods.call
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn- fun-return-type [fun-ty arity]
  (nth (iterate :ret fun-ty) arity))

(defmethod infer/local-infer :call [node context]
  (let [[fn-ty fn-node] (infer/local-infer (:fn node) context)
        args (:args node)
        arg-results (map #(infer/local-infer % context) args)
        arg-tys (map first arg-results)
        arg-nodes (map second arg-results)]
    (if (and fn-ty (every? some? arg-tys) (instance? TFun fn-ty))
      (let [ret-ty (fun-return-type fn-ty (count arg-tys))]
        (infer/success ret-ty
                       (-> node
                           (assoc :fn fn-node :args (vec arg-nodes))
                           (assoc-in [:attrs :type] ret-ty))))
      (infer/nothing (-> node
                         (assoc :fn fn-node :args (vec arg-nodes)))))))