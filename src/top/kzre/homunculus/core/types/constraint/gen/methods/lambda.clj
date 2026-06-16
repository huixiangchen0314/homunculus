(ns top.kzre.homunculus.core.types.constraint.gen.methods.lambda
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.constraint.constraint :as c]
   [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
   [top.kzre.homunculus.core.types.env :as e]
   [top.kzre.homunculus.core.types.protocol :as tp]
   [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :lambda [node context]
  (let [params (n/lambda-params node)
        env (:env context)
        frontend (:frontend context)
        known-types (tp/frontend-types frontend)
        ;; ★ 参数类型：优先使用已设置的类型（如 infer 写入），其次元数据标注，最后新 TVar
        param-tys (mapv (fn [p]
                          (or (ty/get-type p known-types)
                              (gen/fresh-tvar)))
                        params)
        param-names (map n/var-name params)
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        env
                        (map vector param-names param-tys))
        [body-tv body-node body-constr] (gen/cg-node-raw (n/lambda-body node)
                                                         (assoc context :env new-env))
        fn-ty (reduce (fn [ret arg] (ty/make-tfun arg ret)) body-tv (reverse param-tys))
        param-nodes (mapv (fn [p ty] (ty/set-type! p ty)) params param-tys)
        new-node (n/make-lambda param-nodes body-node
                                (n/lambda-captures node) (n/lambda-fn-name node)
                                (n/attrs node) (n/node-meta node) (n/parent node))
        ;; 如果 lambda 自身有标注，生成返回类型约束
        ret-constr (when-let [annotated-ty (ty/meta->type node known-types)]
                     (list (c/make-cequal body-tv annotated-ty)))]
    [fn-ty (ty/set-type! new-node fn-ty)
     (concat body-constr ret-constr)]))