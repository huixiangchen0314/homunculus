(ns top.kzre.homunculus.core.symbol
  "符号表条目构建、访问和判断工具。提供类似 Hiccup 的 DSL 来构建符号表。
   支持同一符号的不同 kind 共存（如 record + function）。"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.core.ir2.pass.type :as ty]
            [top.kzre.homunculus.core.spec :as spec]))

(defn private-symbol?
  [symbol-entry]
  (:private (:meta symbol-entry)))

;; ========== 构建与验证 ==========

(defn- validate! [entry]
  (when-not (s/valid? ::spec/symbol-entry entry)
    (let [explain  (s/explain-str ::spec/symbol-entry entry)]
      (throw (ex-info "Invalid symbol entry"
                      {:explain explain
                       :entry entry}))))
  entry)

(defn- unquote-name [x]
  (if (and (seq? x) (= (first x) 'quote) (= (count x) 2))
    (second x)
    x))

;; ── 构建函数（均保持不变） ──

(defn make-func
  [sym & {:keys [arities params ret rets type meta] :as opts}]
  (let [entry (cond-> {:kind :function :sym sym}
                      type          (assoc :type type)
                      meta          (assoc :meta meta)
                      (seq arities) (assoc :arities (vec arities))
                      (and (empty? arities) (some? params))
                      (assoc :params params :ret ret :rets rets))
        extra (select-keys opts [:io? :pure?])]   ;; 提取允许的属性
    (merge entry extra)))

(defn make-record
  [sym & {:keys [fields protocols type meta] :or {fields [] protocols []}}]
  (let [entry (cond-> {:kind :record :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta)
                      fields (assoc :fields fields)
                      protocols (assoc :protocols protocols))]
    (validate! entry)
    entry))

(defn make-protocol
  [sym & {:keys [methods meta]
          :or {methods []}}]
  (let [entry (cond-> {:kind :protocol :sym sym}
                      meta (assoc :meta meta)
                      methods (assoc :methods methods))]
    (validate! entry)
    entry))

(defn make-variable
  [sym & {:keys [type meta]}]
  (let [entry (cond-> {:kind :variable :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta))]
    (validate! entry)
    entry))

(defn make-primitive
  [sym & {:keys [type meta]}]
  (let [entry (cond-> {:kind :primitive :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta))]
    (validate! entry)
    entry))

;; ── 便捷辅助构建函数 ──

(defn make-param [name & {:keys [type meta]}]
  (cond-> {:param-name name}
          type (assoc :type type)
          meta (assoc :meta meta)))

(defn make-field [name & {:keys [type meta]}]
  (cond-> {:field-name name}
          type (assoc :type type)
          meta (assoc :meta meta)))

(defn make-ret [type & {:keys [meta]}]
  (cond-> {:type type}
          meta (assoc :meta meta)))

(defn make-ret-item [name type & {:keys [meta]}]
  (cond-> {:ret-name name :type type}
          meta (assoc :meta meta)))

(defn make-func-arity [params & {:keys [ret]}]
  (cond-> {:params params}
          ret (assoc :ret ret)))

(defn make-method [name arities]
  {:method-name name :arities (vec arities)})

(def make-method-arity make-func-arity)

(defn make-alias
  [sym target & {:keys [meta]}]
  (let [entry (cond-> {:kind :alias :sym sym :alias-target target}
                      meta (assoc :meta meta))]
    (validate! entry)
    entry))

;; ── 判断函数 ──
(defn alias-symbol? [entry] (= (:kind entry) :alias))

;; ── 访问器 ──
(defn alias-target [entry] (:alias-target entry))

(defn get-type   [entry] (:type entry))


;; ── 解析 DSL 的多方法 ──

(defmulti parse-table-entry
          (fn [entry-vec]
            (let [kind (first entry-vec)]
              kind)))

(defmethod parse-table-entry :func
  [[_ sym & xs]]
  (let [sym          (unquote-name sym)
        [attrs args] (if (map? (first xs))
                       [(first xs) (rest xs)]
                       [{} xs])
        parse-params (fn [param-vec]
                       (mapv (fn [[n t]]
                               (make-param (unquote-name n)
                                           :type (ty/parse-type t)))
                             (partition 2 param-vec)))]
    (cond
      ;; 多重载：args 每个是 [params-vec ret]，且 params-vec 本身是 vector
      (and (seq args) (vector? (first args)) (vector? (ffirst args)))
      (let [arities (mapv (fn [[params ret]]
                            (make-func-arity (parse-params params)
                                             :ret (make-ret (ty/parse-type ret))))
                          args)]
        (list [sym (apply make-func sym :arities arities (apply concat attrs))]))

      ;; 单重载-向量：[params-vec ret]
      (and (seq args) (vector? (first args)))
      (let [[params ret] args]
        (list [sym (apply make-func sym
                          :params (parse-params params)
                          :ret    (make-ret (ty/parse-type ret))
                          (apply concat attrs))]))

      ;; 单重载-打平：n t n t ... ret
      :else
      (let [ret        (last args)
            param-part (butlast args)]
        (list [sym (apply make-func sym
                          :params (parse-params (vec param-part))
                          :ret    (make-ret (ty/parse-type ret))
                          (apply concat attrs))])))))

;; ── 解析 :alias ──
(defmethod parse-table-entry :alias
  [[_ sym target]]
  (let [sym (unquote-name sym)
        target (unquote-name target)]
    (list [sym (make-alias sym target)])))


(defmethod parse-table-entry :record
  [[_ sym & more]]
  (let [sym (unquote-name sym)
        ;; 将 rest 分为字段部分和可选的协议实现部分
        [field-vec proto-impls]
        (loop [remaining more
               fields  []
               protos  []]
          (if (empty? remaining)
            [fields protos]
            (let [fst (first remaining)]
              (if (= fst :protocols)
                ;; 遇到 :protocols 关键字，后面的每个元素是协议实现描述
                (let [proto-descs (rest remaining)]
                  [fields (vec proto-descs)])
                ;; 否则当作字段定义 [field-name type]
                (recur (rest remaining)
                       (conj fields fst)
                       protos)))))
        ;; 解析字段
        fields (mapv (fn [fdef]
                       (let [[fname ftype] fdef
                             fname (unquote-name fname)]
                         (when-not (symbol? fname)
                           (throw (ex-info "Record field name must be a symbol"
                                           {:fname fname :fdef fdef})))
                         (make-field fname :type (ty/parse-type ftype))))
                     field-vec)
        ;; 解析协议实现：每个实现描述形如 [ProtocolName method1 method2 ...]
        protocols (mapv (fn [impl-desc]
                          (let [[proto-name & method-names] impl-desc
                                proto-name (unquote-name proto-name)]
                            {:protocol-name proto-name
                             :impl-method-names (vec method-names)}))
                        proto-impls)
        record-sym sym
        record-entry (make-record record-sym :fields fields :protocols protocols)
        ;; 构造器部分保持不变
        ctor-sym (symbol (str "->" (name record-sym)))
        ctor-params (mapv (fn [field]
                            (make-param (:field-name field) :type (:type field)))
                          fields)
        ret-type (ty/make-tcon record-sym)
        ctor-entry (make-func ctor-sym
                              :params ctor-params
                              :ret (make-ret ret-type))]
    (list [record-sym record-entry]
          [ctor-sym ctor-entry])))

(defmethod parse-table-entry :protocol
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        methods (mapv (fn [[mname & arities]]
                        (make-method (unquote-name mname)
                                     (mapv (fn [[params ret]]
                                             (let [pairs (partition 2 params)
                                                   params (mapv (fn [[n t]]
                                                                  (make-param (unquote-name n) :type (ty/parse-type t)))
                                                                pairs)]
                                               (make-func-arity params :ret (make-ret (ty/parse-type ret)))))
                                           arities)))
                      rest)]
    (list [sym (make-protocol sym :methods methods)])))

(defmethod parse-table-entry :var
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        type (ty/parse-type (first rest))]
    (list [sym (make-variable sym :type type)])))

(defmethod parse-table-entry :primitive
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        type (if (seq rest)
               (ty/parse-type (first rest))
               (ty/make-tcon sym))]
    (list [sym (make-primitive sym :type type)])))

(defmethod parse-table-entry :default
  [entry-vec]
  (throw (ex-info "Unknown entry kind" {:kind (first entry-vec)})))

;; ── 主构建函数：支持同符号多条目合并 ──

(defn entry-domain
  "符号表条目所属的域。

   :value —— 值域（:variable / :function / :alias）
   :type  —— 类型域（:primitive / :record / :protocol）"
  [kind]
  (cond
    (#{:variable :function :alias} kind)         :value
    (#{:primitive :record :protocol} kind)       :type
    :else                                        nil))

(defn entries->overload
  "合并同名条目。

   同域条目冲突——一个符号在值域或类型域中最多一个。
   不同域可共存（例如同名 record + function）。
   冲突时抛异常。"
  [sym existing new-entry]
  (let [existing-entries (if (= :overloaded (:kind existing))
                           (:entries existing)
                           [existing])
        new-domain       (entry-domain (:kind new-entry))]
    (doseq [e existing-entries]
      (when (= (entry-domain (:kind e)) new-domain)
        (throw (ex-info (str "Conflicting symbol entries for " sym
                             ": " (:kind e) " and " (:kind new-entry))
                        {:sym      sym
                         :existing e
                         :new      new-entry}))))
    (if (= :overloaded (:kind existing))
      (update existing :entries conj new-entry)
      {:kind :overloaded :entries [existing new-entry]})))

(defn build-symbol-table [& entries]
  (reduce (fn [table [sym entry :as _]]
            (if-let [old (get table sym)]
              (assoc table sym (entries->overload sym old entry))
              (assoc table sym entry)))
          {}
          (mapcat parse-table-entry entries)))

;; ── 判断函数（单一条目）──

(defn function-symbol? [entry] (= (:kind entry) :function))
(defn record-symbol?   [entry] (= (:kind entry) :record))
(defn protocol-symbol? [entry] (= (:kind entry) :protocol))
(defn variable-symbol? [entry] (= (:kind entry) :variable))
(defn primitive-symbol? [entry] (= (:kind entry) :primitive))

;; ── 工具：从可能的重载条目中筛选指定 kind ──

(defn resolve-overload-entry
  "从条目（可能为重载）中提取指定 kind 的子条目。
   找不到返回 nil。"
  [entry kind]
  (if (= :overloaded (:kind entry))
    (first (filter #(= kind (:kind %)) (:entries entry)))
    (when (= kind (:kind entry)) entry)))

;; ── 特化：从条目（可能为重载）中提取指定 kind 的单个条目 ──

(defn entry->func      [entry] (resolve-overload-entry entry :function))
(defn entry->record    [entry] (resolve-overload-entry entry :record))
(defn entry->protocol  [entry] (resolve-overload-entry entry :protocol))
(defn entry->variable  [entry] (resolve-overload-entry entry :variable))
(defn entry->primitive [entry] (resolve-overload-entry entry :primitive))
(defn entry->alias     [entry] (resolve-overload-entry entry :alias))

(defn resolve-type-entries
  "从条目中解析出所有声明类型的项（:primitive / :record / :protocol）。
   返回向量；无匹配返回空向量。"
  [entry]
  (let [entries (if (= :overloaded (:kind entry))
                  (:entries entry)
                  [entry])]
    (filterv (fn [e] (#{:primitive :record :protocol} (:kind e)))
             entries)))




;; ── 按种类查找（使用新工具）──

(defn lookup-sym
  "在单个表中查找符号 sym，返回可能为单一条目或重载条目，或 nil。"
  [table sym]
  (get table sym))

(defn lookup-func [table sym]
  (loop [visited #{}
         current-sym sym]
    (when-not (contains? visited current-sym)
      (let [visited (conj visited current-sym)
            entry   (lookup-sym table current-sym)
            func    (entry->func entry)]
        (if func
          func
          (when-let [alias (entry->alias entry)]
            (recur visited (alias-target alias))))))))

(defn func-fx?
  [entry]
  (or (:io? entry)
      (not (:pure? entry))))

(defn lookup-record    [table sym] (entry->record    (lookup-sym table sym)))
(defn lookup-protocol  [table sym] (entry->protocol  (lookup-sym table sym)))
(defn lookup-variable  [table sym] (entry->variable  (lookup-sym table sym)))
(defn lookup-primitive [table sym] (entry->primitive (lookup-sym table sym)))

;; ── 其他辅助函数保持不变 ──

(defn lookup-field-type
  [record-entry field-name]
  (when-let [fields (:fields record-entry)]
    (some (fn [f]
            (when (= (:field-name f) field-name)
              (or (:type f)
                  (when-let [tag (:tag (:meta f))]
                    (ty/make-tcon (symbol (name tag)))))))
          fields)))

(defn list-arities
  [func-entry]
  (or (:arities func-entry)
      (when (:params func-entry)
        [(select-keys func-entry [:params :ret])])))


(defn resolve-sym-type
  "查找符号在运行时作为值引用时的类型。

   合法符号：
   - :variable  → 其 :type
   - :function  → 单 arity 时返回其函数类型

   不支持多 arity 函数：多 arity 的函数不具有显式类型，
   必须由重载推断（由调用点提供实参类型后选优），
   因此本函数对多 arity 的项静默返回 nil。

   不处理 alias：alias 在管线初期已被替换展开，
   到达本函数时符号表里不应再有 :alias 条目。
   若遇到 :alias 条目，视为管线配置错误，静默返回 nil。

   注意：本函数不查找编译期类型名（:primitive / :record / :protocol），
   那是类型命名空间的查询，不属于运行时值。"
  [table sym]
  (when-let [entry (lookup-sym table sym)]
    (or (when-let [var-entry (entry->variable entry)]
          (:type var-entry))
        (when-let [func-entry (entry->func entry)]
          (let [arities (list-arities func-entry)]
            (when (= 1 (count arities))
              (ty/arity->tfun (first arities))))))))


(defn find-matching-arities
  [func-entry arg-tys]
  (when-let [arities (list-arities func-entry)]
    (first (filter (fn [arity]
                     (let [params (:params arity)]
                       (and (= (count params) (count arg-tys))
                            (every? identity
                                    (map (fn [p a]
                                           (= (:type p) a))
                                         params arg-tys)))))
                   arities))))

(defn resolve-overload
  [table sym arg-tys]
  (when-let [func-entry (lookup-func table sym)]
    (when-let [arity (find-matching-arities func-entry arg-tys)]
      (when-let [ret (:ret arity)]
        (:type ret)))))

(defn lookup-in-tables
  "按顺序在多个符号表中查找 sym，返回第一个找到的条目（可能为重载条目）。
   支持别名解析：若条目为 :alias，则继续查找其目标符号，直到非别名条目或无结果。"
  [sym & tables]
  (some (fn [tbl]
          (loop [current-sym sym visited #{}]
            (when-let [entry (get tbl current-sym)]
              (if (alias-symbol? entry)
                (let [target (alias-target entry)]
                  (if (contains? visited target)
                    (throw (ex-info "Alias cycle detected" {:sym sym :visited visited}))
                    (recur target (conj visited target))))
                entry))))
        tables))