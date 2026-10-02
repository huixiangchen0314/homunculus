(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.lambda
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :lambda [current-node env]
  (let [params      (n/lambda-params current-node)
        ;; 参数类型：优先已有标注，其次环境中已有的绑定，否则分配新 TVar
        param-types (mapv (fn [param]
                            (or (t/get-type param)
                                (p/resolve-var-type env (n/var-name param))
                                (gen/fresh-tvar)))
                          params)
        ;; 构建函数体内部环境（参数绑定）
        inner-env   (reduce (fn [current-env [param param-type]]
                              (p/bind-var current-env
                                          (n/var-name param)
                                          param-type))
                            env
                            (map vector params param-types))
        ;; 在内部环境中推导函数体
        {:keys [type node constraints]}
        (gen/gen-node* (n/lambda-body current-node) inner-env)
        ;; 构建柯里化函数类型
        fn-type     (reduce (fn [ret arg] (t/make-tfun arg ret))
                            type
                            (reverse param-types))
        ;; 更新参数节点类型并重建 lambda 节点
        param-nodes (mapv (fn [param param-type]
                            (t/set-type! param param-type))
                          params param-types)
        new-node    (n/make-lambda param-nodes node
                                   (n/lambda-captures current-node)
                                   (n/lambda-fn-name current-node)
                                   (n/attrs current-node)
                                   (n/node-meta current-node))
        ;; 若 lambda 整体有标注（如 ^float4），添加约束
        annotated-type (t/get-type current-node)
        ;; lambda 上标注的是函数的返回值类型
        annot-constr   (when annotated-type
                         [(cons/make-cequal (t/fun-return-type fn-type) annotated-type)])]
    {:type        fn-type
     :node        (t/set-type! new-node fn-type)
     :constraints (concat constraints annot-constr)
     ;; 返回外部环境——函数内部定义的类型不泄露
     :env         env}))