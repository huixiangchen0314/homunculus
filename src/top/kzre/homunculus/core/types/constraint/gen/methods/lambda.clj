(ns top.kzre.homunculus.core.types.constraint.gen.methods.lambda
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.utils :as u]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :lambda [node context]
  (let [params      (n/lambda-params node)
        known-types (u/known-types context)
        ;; 获取每个参数的类型：优先已有标注，其次当前环境（可能已经绑定了变量名?），否则分配新 TVar
        param-tys   (mapv (fn [p]
                            (or (t/get-type p known-types)
                                (e/lookup-env (u/env context) (n/var-name p))
                                (gen/fresh-tvar)))
                          params)
        param-names (map n/var-name params)
        ;; 构建函数体内部环境（参数绑定）
        inner-env   (reduce (fn [env [name ty]] (e/extend-env env name ty))
                            (u/env context)
                            (map vector param-names param-tys))
        ;; 在内部环境中推导函数体
        [body-tv body-node body-constr _body-ctx] (gen/cg-node-raw (n/lambda-body node)
                                                                   (assoc context :env inner-env))
        ;; 构建柯里化函数类型
        fn-ty       (reduce (fn [ret arg] (t/make-tfun arg ret)) body-tv (reverse param-tys))
        ;; 更新参数节点类型并重建 lambda 节点
        param-nodes (mapv (fn [p ty] (t/set-type! p ty)) params param-tys)
        new-node    (n/make-lambda param-nodes body-node
                                   (n/lambda-captures node) (n/lambda-fn-name node)
                                   (n/attrs node) (n/node-meta node) (n/parent node))
        ;; 约束：返回值类型 = 函数体类型
        ret-constr  (when body-tv [(c/make-cequal (t/fun-ret fn-ty) body-tv)])
        ;; 若 lambda 整体有标注（如 ^float4），添加约束
        annotated-ty (t/get-type node known-types)
        ;; lambda 上标注的是函数的返回值类型.
        annot-constr (when annotated-ty [(c/make-cequal (t/fun-return-type fn-ty) annotated-ty)])]
    [fn-ty (t/set-type! new-node fn-ty)
     (concat body-constr ret-constr annot-constr)
     ;; 返回外部上下文，函数内部定义的类型不泄露
     context]))