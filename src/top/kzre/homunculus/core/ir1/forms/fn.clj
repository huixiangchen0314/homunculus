(ns top.kzre.homunculus.core.ir1.forms.fn
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'fn [form env]
  (let [[fn-sym maybe-name params & body] form
        [name param-v body-exprs] (if (symbol? maybe-name)
                                    [maybe-name params body]
                                    [nil maybe-name (cons params body)])
        env-outer  env
        env-fn     (if name (ir1/bind-local-symbol env name) env)
        env-params (reduce (fn [e p] (ir1/bind-local-symbol e p)) env-fn param-v)
        param-descs (mapv (fn [p] (n/make-param p (meta p))) param-v)
        [body-nodes _] (ir1/lower-seq body-exprs env-params)
        merged-meta (merge (meta fn-sym) (meta form) (meta param-v))]
    [(n/make-fn name param-descs (n/wrap-body body-nodes) merged-meta)
     env-outer]))