(ns top.kzre.homunculus.core.ir1.forms.protocol
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]))

;; 方法名属于协议短名，保持裸符号；下游组合 Protocol.name + Method.name。
(defn- parse-single-method [method-form]
  (let [[method-name param-vec & more] method-form
        docstring   (when (string? (first more)) (first more))
        method-meta (merge (meta method-name) (meta method-form) (meta param-vec))
        params      (rest param-vec)
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) params)]
    (m/->Method method-name param-nodes nil docstring method-meta)))

(defn- parse-protocol-methods [methods]
  (mapv parse-single-method methods))

(defmethod ir1/lower-node* 'defprotocol [form env]
  (let [[_ proto-sym & methods] form
        protocol-name (with-meta (ir1/qualify-ns-symbol env proto-sym) (meta proto-sym))
        env'          (ir1/bind-global-symbol env protocol-name)
        method-nodes  (parse-protocol-methods methods)
        protocol-meta (merge (meta form) (meta proto-sym))]
    [(m/->Protocol protocol-name method-nodes protocol-meta) env']))