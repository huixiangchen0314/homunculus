(ns top.kzre.homunculus.core.types.infer.methods.call
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type])
  (:import (top.kzre.homunculus.core.types.model TFun)))

(defn- fun-return-type [fun-ty arity]
  (nth (iterate :ret fun-ty) arity))

(defmethod infer/local-infer :call [node context]
  (let [[fn-ty fn-node] (infer/local-infer (:fn node) context)
        args (:args node)
        arg-results (map #(infer/local-infer % context) args)
        arg-tys (map first arg-results)
        arg-nodes (map second arg-results)]
    (if (and fn-ty (every? some? arg-tys) (instance? TFun fn-ty))
      (let [ret-ty (fun-return-type fn-ty (count arg-tys))
            ;; 更新节点：先设置子节点，再强制设置类型
            updated-node (-> node
                             (assoc :fn fn-node :args (vec arg-nodes))
                             (type/set-type! ret-ty))]
        (infer/success ret-ty updated-node))
      (infer/nothing (-> node
                         (assoc :fn fn-node :args (vec arg-nodes)))))))