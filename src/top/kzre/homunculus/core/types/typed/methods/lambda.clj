(ns top.kzre.homunculus.core.types.typed.methods.lambda
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :lambda [node context]
  (let [params (:params node)   ;; [:variable ...]
        param-tys (repeatedly (count params) #(t/->TVar (gensym "p")))
        param-names (map :name params)
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        (:env context)
                        (map vector param-names param-tys))
        [body-ty body-node] (infer/infer (:body node) (assoc context :env new-env))
        fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-ty (reverse param-tys))
        param-nodes (map (fn [p ty] (assoc-in p [:attrs :type] ty)) params param-tys)
        new-attrs (assoc (ir2p/attrs node) :type fn-ty)
        new-node (assoc node :params (vec param-nodes) :body body-node :attrs new-attrs)]
    [fn-ty new-node]))