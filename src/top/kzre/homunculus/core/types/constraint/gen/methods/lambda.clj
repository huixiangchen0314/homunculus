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

        param-tys (mapv (fn [p]
                          (or (ty/get-type p known-types)           ;; 已有类型 (infer 写入)
                              (e/lookup-env env (n/var-name p))    ;; 本地环境
                              (gen/fresh-tvar)))                   ;; 分配新 TVar
                        params)
        param-names (map n/var-name params)
        ;; 扩展环境，使参数名在函数体内可见
        new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                        env
                        (map vector param-names param-tys))
        ;; 推导函数体
        [body-tv body-node body-constr] (gen/cg-node-raw (n/lambda-body node)
                                                         (assoc context :env new-env))
        ;; 构建函数类型：参数类型 -> 返回值类型
        fn-ty (reduce (fn [ret arg] (ty/make-tfun arg ret)) body-tv (reverse param-tys))
        param-nodes (mapv (fn [p ty] (ty/set-type! p ty)) params param-tys)
        new-node (n/make-lambda param-nodes body-node
                                (n/lambda-captures node) (n/lambda-fn-name node)
                                (n/attrs node) (n/node-meta node) (n/parent node))
        ;; 生成约束，将 lambda 的返回值类型与函数体返回值类型绑定
        ret-constr (list (c/make-cequal (ty/fun-ret fn-ty) body-tv))
        ;; 若 lambda 自身有标注（如 ^:float4），也生成约束
        annotated-constr (when-let [annotated-ty (ty/meta->type node known-types)]
                           (list (c/make-cequal (ty/fun-ret fn-ty) annotated-ty)))]
    [fn-ty (ty/set-type! new-node fn-ty)
     (concat body-constr ret-constr annotated-constr)]))