(ns top.kzre.homunculus.core.ir1.forms.record
  "defrecord 的 IR1 构建。"
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]))

;; ── 解析字段（仅保留符号，忽略默认值） ──
(defn- parse-fields [field-vec]
  (->> field-vec
       (filter symbol?)
       (mapv (fn [sym] (m/->Field sym (meta sym))))))

;; ── 解析单个方法定义 ──────────────────────
;; 方法体是封闭作用域，env' 内部丢弃，不影响 defrecord 外部。
(defn- parse-method [method-form env]
  (let [[method-name params-vec & body-exprs] method-form
        docstring   (when (string? (first body-exprs)) (first body-exprs))
        body-forms  (if docstring (rest body-exprs) body-exprs)
        method-meta (merge (meta method-name) (meta method-form) (meta params-vec))
        param-nodes (mapv (fn [p] (m/->Param p (meta p))) (rest params-vec))
        [body-nodes _] (ir1/lower-seq body-forms env)
        ir-body     (n/wrap-body body-nodes (meta body-forms))]
    (m/->Method method-name param-nodes ir-body docstring method-meta)))

;; ── 解析协议实现 ──────────────────────────
(defn- parse-protocols [body-forms env]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)
        (recur (rest forms)
               {:proto-name form :methods []}
               (if current-proto (conj result current-proto) result))
        (let [method (parse-method form env)]
          (if current-proto
            (recur (rest forms)
                   (update current-proto :methods conj method)
                   result)
            (throw (ex-info "Method without protocol" {:method form})))))
      (if current-proto (conj result current-proto) result))))

;; ── form->node ────────────────────────────
(defmethod ir1/lower-node* 'defrecord [form env]
  (let [[_ name field-vec & body-forms] form
        fields         (parse-fields field-vec)
        proto-maps     (parse-protocols body-forms env)
        protocol-impls (mapv (fn [pm]
                               (m/->ProtocolImpl (:proto-name pm)
                                                 (:methods pm)
                                                 (meta form)))
                             proto-maps)]
    [(m/->Record name fields protocol-impls (meta form)) env]))