(ns top.kzre.homunculus.core.ir1.forms.fn
  "fn 特殊形式的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; fn 作为特殊形式，不展开。
(defmethod ir1/form->node 'fn [form]
  (let [[fn-sym maybe-name params & body] form
        [name param-v body] (if (symbol? maybe-name)
                              [maybe-name params body]
                              [nil maybe-name (cons params body)])
        param-descs  (mapv (fn [p] (n/make-param p (meta p))) param-v)
        merged-meta  (merge (meta fn-sym) (meta form) (meta param-v))]
    (n/make-fn name
               param-descs
               (n/wrap-body (mapv ir1/->ir1 body))
               merged-meta)))