(ns top.kzre.homunculus.core.ir1.forms.record
  "defrecord 的 IR1 构建。使用新的 Field / Method / ProtocolImpl 节点。"
  (:require
   [top.kzre.homunculus.core.ir1.core :as ir1]
   [top.kzre.homunculus.core.ir1.model :as m]
   [top.kzre.homunculus.core.ir1.node :as n]))

;; ── 解析字段（仅保留符号，忽略默认值） ──
(defn- parse-fields [field-vec]
  (->> field-vec
       (filter symbol?)
       (mapv (fn [sym] (m/->Field sym (meta sym))))))

;; ── 解析单个方法定义 ──────────────────────
(defn- parse-method [method-form]
  ;; 形式：(method-name [this & params] body...)
  (let [[method-name params-vec & body-exprs] method-form
        docstring (when (string? (first body-exprs)) (first body-exprs))
        body-forms (if docstring (rest body-exprs) body-exprs)
        method-meta (merge (meta method-name) (meta method-form) (meta params-vec))
        ;; 参数跳过 this，并转为 Param 节点
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) (rest params-vec))
        ;; 递归转换方法体表达式
        ir-body (when (seq body-forms)
                  (let [nodes (mapv ir1/->ir1 body-forms)]
                    (n/wrap-body nodes (meta body-forms))))]
    (m/->Method method-name param-nodes ir-body docstring method-meta)))

;; ── 解析协议实现 ──────────────────────────
(defn- parse-protocols [body-forms]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)   ;; 协议名
        (recur (rest forms)
               {:proto-name form :methods []}
               (if current-proto (conj result current-proto) result))
        ;; 方法定义
        (let [method (parse-method form)]
          (if current-proto
            (recur (rest forms)
                   (update current-proto :methods conj method)
                   result)
            (throw (ex-info "Method without protocol" {:method form})))))
      ;; 结束，将最后一个协议实现加入结果
      (if current-proto (conj result current-proto) result))))

;; ── form->node：一次性递归构建完整 IR1 ──
(defmethod ir1/form->node 'defrecord [form]
  (let [[_ name field-vec & body-forms] form
        fields     (parse-fields field-vec)
        proto-maps (parse-protocols body-forms)
        ;; 转换为 ProtocolImpl 节点
        protocol-impls (mapv (fn [pm]
                               (m/->ProtocolImpl (:proto-name pm)
                                                 (:methods pm)
                                                 (meta form))) ; 使用 defrecord 的 meta
                             proto-maps)]
    ;; name 保持原始符号，meta 使用整个 form 的 meta
    (m/->Record name fields protocol-impls (meta form))))