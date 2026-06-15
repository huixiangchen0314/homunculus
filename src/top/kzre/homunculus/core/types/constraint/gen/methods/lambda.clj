(ns top.kzre.homunculus.core.types.constraint.gen.methods.lambda
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :lambda [node context]
  (let [params (:params node)
        env (:env context)
        param-tys (mapv (fn [p] (or (ty/get-type p) (gen/fresh-tvar))) params)
        param-names (map :name params)
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        env
                        (map vector param-names param-tys))
        [body-tv body-node body-constr] (gen/cg-node-raw (:body node) (assoc context :env new-env))
        fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-tv (reverse param-tys))
        param-nodes (mapv (fn [p ty] (ty/set-type! p ty)) params param-tys)
        new-node (m/->LambdaNode param-nodes body-node (:captures node) (:fn-name node)
                                 (:attrs node) (:meta node) (:parent node))]
    [fn-ty (ty/set-type! new-node fn-ty) body-constr]))