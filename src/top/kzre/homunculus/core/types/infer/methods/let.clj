(ns top.kzre.homunculus.core.types.infer.methods.let
  (:require [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :let [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env]
        (reduce (fn [[bnds env] [var-node val-node]]
                  (let [[val-ty val-new] (infer/local-infer val-node (assoc context :env env))
                        var-name (:name var-node)
                        env2 (if val-ty (e/extend-env env var-name val-ty) env)
                        ;; 使用 ensure-type 保护已有的类型标注
                        var-new (if val-ty
                                  (type/ensure-type var-node val-ty)
                                  var-node)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] (:env context)]
                bindings)
        [body-ty body-node] (infer/local-infer (:body node) (assoc context :env new-env))]
    (if body-ty
      (infer/success body-ty
                     ;; 使用 set-type! 强制写入推断出的整体类型
                     (type/set-type! (assoc node :bindings (vec bind-nodes) :body body-node)
                                     body-ty))
      (infer/nothing (assoc node :bindings (vec bind-nodes) :body body-node)))))