(ns top.kzre.homunculus.core.ir1.forms.record
  "defrecord 的 IR1 构建。所有节点字段访问均通过 ir1.node 工具函数。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── 解析方法体 ────────────────────────────
(defn- parse-method-bodies [method-form]
  (let [method-name (first method-form)
        arities     (rest method-form)]
    (mapv (fn [arity-form]
            (let [[this-sym & params] (first arity-form)
                  new-params (mapv (fn [p] (n/make-param p (meta p))) params)
                  body-exprs (rest arity-form)          ;; 剩余表单是方法体
                  body       (n/wrap-body body-exprs)
                  arity-meta (meta arity-form)]
              (n/make-arity method-name new-params body arity-meta)))
          arities)))

;; ── 解析协议实现 ──────────────────────────
(defn- parse-protocols [body-forms]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)                     ;; 协议名
        (recur (rest forms)
               (n/make-protocol-impl form [])
               (if current-proto
                 (conj result current-proto)
                 result))
        (let [method-arities (parse-method-bodies form)]
          (if current-proto
            (recur (rest forms)
                   (n/protocol-impl-add-methods current-proto method-arities)
                   result)
            (throw (ex-info "Method without protocol" {:method form})))))
      ;; 结束，加入最后一个协议实现
      (if current-proto
        (conj result current-proto)
        result))))

;; ── 解析字段（支持默认值）────────────────
(defn- parse-fields [field-vec]
  (loop [items (seq field-vec)
         result []]
    (if items
      (let [f (first items)]
        (if (symbol? f)                     ;; 只有名字，无默认值
          (recur (rest items)
                 (conj result (n/make-field f nil (meta f))))
          (let [default-expr (second items)]
            (recur (nnext items)
                   (conj result (n/make-field f default-expr (meta f)))))))
      result)))

;; ── form->node ：只提取原始数据 ───────────
(defmethod ir1/form->node 'defrecord [form]
  (let [[_ name field-vec & body-forms] form
        fields    (parse-fields field-vec)
        protocols (parse-protocols body-forms)]
    (m/->RecordNode name fields protocols (meta form) nil)))

;; ── build-tree ：递归构建 IR1 子树 ───────
(defmethod ir1/build-tree :record [node]
  (let [name      (n/record-name node)                      ;; 保持简单符号
        fields    (n/record-fields node)
        protocols (n/record-protocols node)
        meta      (n/node-meta node)
        parent    (n/parent node)
        ;; 转换字段默认值
        new-fields (mapv (fn [f]
                           (if-let [init (n/field-init f)]
                             (n/field-with-init f (ir1/->ir1 init))
                             f))
                         fields)
        ;; 转换协议方法体
        new-protocols (mapv
                        (fn [impl]
                          (n/protocol-impl-map-methods
                            impl
                            (fn [m]
                              (if-let [body (n/arity-body m)]
                                (n/arity-with-body m (ir1/->ir1 body))
                                m))))
                        protocols)]
    (m/->RecordNode name new-fields new-protocols meta parent)))