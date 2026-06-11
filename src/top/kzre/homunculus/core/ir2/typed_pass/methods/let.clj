(ns top.kzre.homunculus.core.ir2.typed-pass.methods.let
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e]))

(defmethod infer/infer :let [node env]
  (let [bindings (:bindings node)
        [bind-nodes env']
        (reduce (fn [[bnds env] [var-node val-node]]
                  (let [[val-ty val-new] (infer/infer val-node env)
                        annot-ty (t/meta-type var-node)
                        ;; 若存在用户标注，直接使用标注类型，不统一值类型
                        var-ty (or annot-ty val-ty)
                        var-name (:name var-node)
                        env2 (e/extend-env env var-name var-ty)
                        var-new (assoc-in var-node [:attrs :type] var-ty)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] env] bindings)
        [body-ty body-node] (infer/infer (:body node) env')]
    [body-ty (assoc node :bindings (vec bind-nodes) :body body-node
                         :attrs (assoc (:attrs node) :type body-ty))]))