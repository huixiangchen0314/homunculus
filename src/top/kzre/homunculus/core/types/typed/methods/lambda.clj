(ns top.kzre.homunculus.core.types.typed.methods.lambda
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :lambda [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [params (:params node)
          param-tys (repeatedly (count params) #(t/->TVar (gensym "p")))
          param-names (map :name params)
          new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                          (:env context)
                          (map vector param-names param-tys))
          [body-ty body-node s-body] (infer/infer (:body node) (assoc context :env new-env))
          ;; 应用替换到参数类型和返回类型
          param-tys' (mapv #(u/substitute % s-body) param-tys)
          body-ty'   (u/substitute body-ty s-body)
          ;; 构造应用了替换的函数类型
          fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-ty' (reverse param-tys'))
          ;; 更新参数节点的类型（应用替换后）
          param-nodes (map (fn [p ty] (assoc-in p [:attrs :type] ty)) params param-tys')
          new-attrs (assoc (ir2p/attrs node) :type fn-ty)]
      [fn-ty (assoc node :params (vec param-nodes) :body body-node :attrs new-attrs) s-body])))