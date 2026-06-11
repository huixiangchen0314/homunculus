(ns top.kzre.homunculus.core.ir2.typed-pass.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e]))

(defmethod infer/infer :lambda [node env]
  (let [params (:params node)
        param-tys (repeatedly (count params) t/fresh-tvar)
        param-names (map :name params)
        env' (reduce (fn [acc [name ty]] (e/extend-env acc name ty))
                     env (map vector param-names param-tys))
        [body-ty body-node] (infer/infer (:body node) env')
        fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-ty (reverse param-tys))
        param-new (map (fn [p ty] (assoc-in p [:attrs :type] ty)) params param-tys)]
    [fn-ty (assoc node :params (vec param-new) :body body-node
                       :attrs (assoc (:attrs node) :type fn-ty))]))