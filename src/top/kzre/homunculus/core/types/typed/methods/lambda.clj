(ns top.kzre.homunculus.core.types.typed.methods.lambda
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :lambda [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [params (:params node)
          known-types (:known-types context)
          ;; 优先使用已有标注的类型，否则生成 TVar
          param-tys (mapv (fn [p]
                            (if (type/has-type? p known-types)
                              (type/get-type p known-types)
                              (t/->TVar (gensym "p"))))
                          params)
          param-names (map :name params)
          new-env (reduce (fn [env [name ty]] (e/extend-env env name ty))
                          (:env context)
                          (map vector param-names param-tys))
          [body-ty body-node s-body] (infer/infer (:body node) (assoc context :env new-env))
          param-tys' (mapv #(u/substitute % s-body) param-tys)
          body-ty'   (u/substitute body-ty s-body)
          fn-ty (reduce (fn [ret arg] (t/->TFun arg ret)) body-ty' (reverse param-tys'))
          ;; 强制更新参数节点的类型（应用替换后）
          param-nodes (map (fn [p ty] (type/set-type! p ty)) params param-tys')
          ;; 强制更新 lambda 节点的类型
          updated-node (-> node
                           (assoc :params (vec param-nodes) :body body-node)
                           (type/set-type! fn-ty))]
      [fn-ty updated-node s-body])))