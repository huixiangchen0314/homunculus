(ns top.kzre.homunculus.core.ir1.parse
  "解析 Clojure 原始表单为 IR1 节点树。
   顶层 parse 返回节点向量，不进行包装。"
  (:require [top.kzre.homunculus.core.ir1.ast :as ast]))

;; ── 辅助 ──────────────────────────────────
(defn- special? [op]
  (and (symbol? op) (contains? ast/special-forms op)))


(defn wrap-exprs [exprs]
  (case (count exprs)
    0 nil
    1 (first exprs)
    (ast/->Do exprs nil)))

;; ── 主分派 ────────────────────────────────
(defmulti parse-form
          (fn [form]
            (cond
              (nil? form)       :literal
              (boolean? form)   :literal
              (number? form)    :literal
              (string? form)    :literal
              (char? form)      :literal
              (keyword? form)   :literal
              (symbol? form)    :symbol
              (vector? form)    :vector
              (map? form)       :map
              (seq? form)       (let [op (first form)]
                                  (cond
                                    (keyword? op) :keyword-access
                                    (= op '.)     :dot
                                    (special? op) op
                                    :else         :call))
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))

(defn- parse-forms [forms]
  (mapv parse-form forms))

;; ── 简单节点 ─────────────────────────────
(defmethod parse-form :literal [form]
  (ast/->Literal form (meta form)))

(defmethod parse-form :symbol [form]
  (ast/->Symbol form (meta form)))

(defmethod parse-form :vector [form]
  (ast/->Vector (parse-forms form) (meta form)))

(defmethod parse-form :map [form]
  (let [pairs (mapcat (fn [[k v]]
                        [(ast/->Pair (parse-form k) (parse-form v) nil)])
                      form)]
    (ast/->Map (vec pairs) (meta form))))

;; ── 调用 ─────────────────────────────────
(defmethod parse-form :call [form]
  (let [[op & args] form
        fn-node (parse-form op)
        arg-nodes (parse-forms args)]
    (ast/->Call fn-node arg-nodes (meta form))))

;; ── 特殊形式 ─────────────────────────────
(defmethod parse-form 'if [form]
  (let [[_ test then else] form]
    (ast/->If (parse-form test)
              (parse-form then)
              (some-> else parse-form)
              (meta form))))

(defmethod parse-form 'do [form]
  (let [[_ & exprs] form
        nodes (parse-forms exprs)]
    (wrap-exprs nodes)))

(defmethod parse-form 'let [form]
  (let [[_ bindings & body] form
        pairs (partition 2 bindings)
        ir-bindings (mapv (fn [[sym val]]
                            (ast/->Binding (parse-form sym) (parse-form val) nil))
                          pairs)
        ir-body (wrap-exprs (parse-forms body))]
    (ast/->Let ir-bindings ir-body (meta form))))

(defmethod parse-form 'loop [form]
  (let [[_ bindings & body] form
        pairs (partition 2 bindings)
        ir-bindings (mapv (fn [[sym val]]
                            (ast/->Binding (parse-form sym) (parse-form val) nil))
                          pairs)
        ir-body (wrap-exprs (parse-forms body))]
    (ast/->Loop ir-bindings ir-body (meta form))))

(defmethod parse-form 'fn [form]
  (let [[_ maybe-name params & body] form
        [name params body] (if (symbol? maybe-name)
                             [maybe-name params body]
                             [nil maybe-name (cons params body)])
        param-nodes (mapv (fn [p]
                            (ast/->Param (parse-form p) nil))
                          params)
        ir-body (wrap-exprs (parse-forms body))]
    (ast/->Fn name param-nodes ir-body (merge (meta form) (meta params)))))

(defmethod parse-form 'def [form]
  (let [[_ sym & more] form
        docstring (when (string? (first more)) (first more))
        more (if docstring (rest more) more)
        attr-map (when (map? (first more)) (first more))
        val-expr (if attr-map (second more) (first more))
        ;; 将 attr-map 合并到 meta
        meta (merge (meta form) (meta sym) attr-map)]
    (ast/->Def (parse-form sym) docstring (parse-form val-expr) meta)))

(defmethod parse-form 'recur [form]
  (let [[_ & exprs] form]
    (ast/->Recur (parse-forms exprs) (meta form))))

(defmethod parse-form 'quote [form]
  (let [[_ expr] form]
    (ast/->Quote (parse-form expr) (meta form))))

(defmethod parse-form 'var [form]
  (let [[_ sym] form]
    (ast/->Var (parse-form sym) (meta form))))

(defmethod parse-form 'throw [form]
  (let [[_ expr] form]
    (ast/->Throw (parse-form expr) (meta form))))

(defmethod parse-form 'set! [form]
  (let [[_ var val] form]
    (ast/->Set (parse-form var) (parse-form val) (meta form))))

;; ── try / catch / finally ────────────────
(defmethod parse-form 'try [form]
  (let [[_ & body-parts] form
        body (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body (drop (count body) body-parts)
        catches (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (= 'finally (ffirst finally-part))
                       (rest (first finally-part)))
        ir-body (wrap-exprs (parse-forms body))
        ir-catches (mapv (fn [clause]
                           (let [[_ class sym & cbody] clause]
                             (ast/->Catch class sym (parse-forms cbody) (meta clause))))
                         catches)
        ir-finally (when finally-expr
                     (wrap-exprs (parse-forms finally-expr)))]
    (ast/->Try ir-body ir-catches ir-finally (meta form))))

;; ── defrecord ────────────────────────────
(defn- parse-fields [field-vec]
  (loop [items (seq field-vec)
         result []]
    (if-let [f (first items)]
      (if (symbol? f)
        (let [rest-items (rest items)
              next (first rest-items)]
          (if (and next (not (symbol? next)))
            ;; 有默认值表达式
            (recur (rest rest-items)
                   (conj result (ast/->RecordField f (parse-form next) (meta f))))
            ;; 无默认值
            (recur rest-items
                   (conj result (ast/->RecordField f nil (meta f))))))
        (throw (ex-info "Record field name must be a symbol" {:found f})))
      result)))

(defn- parse-protocol-impls [body-forms]
  (loop [forms body-forms
         current-proto nil
         result []]
    (if-let [form (first forms)]
      (if (symbol? form)
        (recur (rest forms)
               form   ;; 协议符号
               (if current-proto
                 (conj result current-proto)
                 result))
        ;; 方法形式
        (let [[method-name & arities] form
              methods (mapv (fn [arity-form]
                              (let [[_this-sym & params] (first arity-form)
                                    body-exprs (rest arity-form)
                                    param-nodes (mapv #(ast/->Param (parse-form %) nil) params)
                                    body (wrap-exprs (parse-forms body-exprs))]
                                (ast/->ProtocolMethod method-name param-nodes body (meta arity-form))))
                            arities)]
          (if current-proto
            (let [new-impl (ast/->ProtocolImpl (ast/->Symbol current-proto nil) methods nil)]
              (recur (rest forms)
                     current-proto
                     (conj result new-impl)))
            (throw (ex-info "Method without protocol" {:method form})))))
      (if current-proto
        (conj result current-proto)
        result))))

(defmethod parse-form 'defrecord [form]
  (let [[_ name field-vec & body-forms] form
        fields (parse-fields field-vec)
        protocol-impls (parse-protocol-impls body-forms)]
    (ast/->Record name fields protocol-impls (meta form))))

;; ── defprotocol ──────────────────────────
(defmethod parse-form 'defprotocol [form]
  (let [[_ name & methods] form]
    ;; funcs 保留为属性，不作深层解析
    (ast/->Protocol name methods (meta form))))

;; ── ns ───────────────────────────────────
(defmethod parse-form 'ns [form]
  (let [[_ name & rest-args] form
        docstring (when (string? (first rest-args)) (first rest-args))
        after-doc (if docstring (rest rest-args) rest-args)
        attr-map (when (map? (first after-doc)) (first after-doc))
        ;; 提取 :require 子句，并去掉 :require 符号本身
        requires (when-let [req-clause (first (if attr-map (rest after-doc) after-doc))]
                   (when (and (sequential? req-clause) (= :require (first req-clause)))
                     (vec (rest req-clause))))]
    (ast/->Ns name docstring requires (merge (meta form) attr-map))))

;; ── member-access (. obj method args) 和 (:key obj) ──
(defmethod parse-form :dot [form]
  (let [[_ target member & args] form]
    (ast/->MemberAccess (parse-form target)
                        member
                        (parse-forms args)
                        (meta form))))

(defmethod parse-form :keyword-access [form]
  (let [[k obj] form]
    (ast/->MemberAccess (parse-form obj)
                        k
                        []  ;; 关键字访问无额外参数
                        (meta form))))

;; ── 顶层解析入口 ─────────────────────────
(defn parse
  "将一系列 Clojure 顶层表单解析为 IR1 节点向量。"
  [forms]
  (parse-forms forms))