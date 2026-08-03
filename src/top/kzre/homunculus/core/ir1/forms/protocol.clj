(ns top.kzre.homunculus.core.ir1.forms.protocol
  "defprotocol 的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defn- parse-single-method [method-form]
  ;; 解析单个方法： (name [this & params] docstring?)
  (let [[method-name param-vec & more] method-form
        docstring (when (string? (first more)) (first more))
        ;; 合并方法名、参数向量、整个表单的元数据
        method-meta (merge (meta method-name)
                           (meta param-vec)
                           (meta method-form))
        ;; 移除 this，保留剩余参数
        params (rest param-vec)
        ;; 参数符号 → Param 节点，attrs 为空，保留参数自身的元数据
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) params)]
    (m/->Method method-name param-nodes docstring method-meta)))

(defn- parse-protocol-methods [methods]
  (mapv parse-single-method methods))

(defmethod ir1/form->node 'defprotocol [form]
  (let [[_ name & methods] form
        method-nodes (parse-protocol-methods methods)
        ;; 协议名称保持原始符号，不转为节点
        protocol-meta (merge (meta form) (meta name))]
    (m/->Protocol name method-nodes protocol-meta)))