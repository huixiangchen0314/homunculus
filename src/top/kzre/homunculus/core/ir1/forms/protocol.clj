(ns top.kzre.homunculus.core.ir1.forms.protocol
  "defprotocol 的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]))

(defn- parse-single-method [method-form]
  ;; 解析单个方法： (name [this & params] docstring?)
  (let [[method-name param-vec & more] method-form
        docstring   (when (string? (first more)) (first more))
        method-meta (merge (meta method-name)
                           (meta method-form)
                           (meta param-vec))
        params      (rest param-vec)
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) params)]
    (m/->Method method-name param-nodes nil docstring method-meta)))

(defn- parse-protocol-methods [methods]
  (mapv parse-single-method methods))

(defmethod ir1/lower-node* 'defprotocol [form env]
  (let [[_ name & methods] form
        method-nodes  (parse-protocol-methods methods)
        protocol-meta (merge (meta form) (meta name))]
    [(m/->Protocol name method-nodes protocol-meta) env]))