(ns top.kzre.homunculus.internal.symbol
  "符号表条目构建、访问和判断工具。提供类似 Hiccup 的 DSL 来构建符号表。
   支持同一符号的不同 kind 共存（如 record + function）。"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.internal.spec :as spec]))

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

(defn- parse-type [type-spec]
  (cond
    (keyword? type-spec) (ty/make-tcon (symbol (name type-spec)))
    (symbol? type-spec)  (ty/make-tcon type-spec)
    (vector? type-spec)
    (let [parts (partition-by #{'->} type-spec)
          types (remove #{['->]} parts)]
      (reduce (fn [ret arg]
                (ty/make-tfun (parse-type (first arg)) ret))
              (parse-type (last (last types)))
              (reverse (butlast types))))
    :else type-spec))

;; ── 构建函数（均保持不变） ──

(defn make-func
  [sym & {:keys [arities params ret rets type meta] :or {arities []}}]
  (let [entry (cond-> {:kind :function :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta)
                      (seq arities) (assoc :arities (vec arities))
                      (and (empty? arities) (some? params))
                      (assoc :params params :ret ret :rets rets))]
    (validate! entry)
    entry))

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
  [sym & {:keys [methods type meta] :or {methods []}}]
  (let [entry (cond-> {:kind :protocol :sym sym}
                      type (assoc :type type)
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

;; ── 访问函数 ──

(defn get-kind   [entry] (:kind entry))
(defn get-type   [entry] (:type entry))
(defn get-meta   [entry] (:meta entry))
(defn get-fields [entry] (:fields entry))
(defn get-protocols [entry] (:protocols entry))
(defn get-methods [entry] (:methods entry))
(defn get-params [entry] (:params entry))
(defn get-ret    [entry] (:ret entry))
(defn get-rets   [entry] (:rets entry))

(defn get-arities [entry]
  (:arities entry))

;; ── 解析 DSL 的多方法 ──

(defmulti parse-table-entry
          (fn [entry-vec]
            (let [kind (first entry-vec)]
              kind)))

(defmethod parse-table-entry :func
  [[_ sym & rest]]
  (let [sym (unquote-name sym)]
    (if (and (vector? (first rest))
             (vector? (ffirst rest)))
      ;; 多重载
      (let [arities (mapv (fn [arity]
                            (let [[params ret] arity
                                  pairs (partition 2 params)
                                  params (mapv (fn [[n t]]
                                                 (make-param (unquote-name n) :type (parse-type t)))
                                               pairs)]
                              (make-func-arity params :ret (make-ret (parse-type ret)))))
                          rest)]
        (list [sym (make-func sym :arities arities)]))
      ;; 单重载
      (let [ret (last rest)
            param-pairs (partition 2 (butlast rest))
            params (mapv (fn [[n t]]
                           (make-param (unquote-name n) :type (parse-type t)))
                         param-pairs)]
        (list [sym (make-func sym :params params :ret (make-ret (parse-type ret)))])))))

(defmethod parse-table-entry :record
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        fields (mapv (fn [fdef]
                       (let [[fname ftype] fdef
                             fname (unquote-name fname)]
                         (when-not (symbol? fname)
                           (throw (ex-info "Record field name must be a symbol"
                                           {:fname fname :fdef fdef})))
                         (make-field fname :type (parse-type ftype))))
                     rest)
        record-sym sym
        record-entry (make-record record-sym :fields fields)
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
                                                                  (make-param (unquote-name n) :type (parse-type t)))
                                                                pairs)]
                                               (make-func-arity params :ret (make-ret (parse-type ret)))))
                                           arities)))
                      rest)]
    (list [sym (make-protocol sym :methods methods)])))

(defmethod parse-table-entry :var
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        type (parse-type (first rest))]
    (list [sym (make-variable sym :type type)])))

(defmethod parse-table-entry :primitive
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        type (if (seq rest)
               (parse-type (first rest))
               (ty/make-tcon sym))]
    (list [sym (make-primitive sym :type type)])))

(defmethod parse-table-entry :default
  [entry-vec]
  (throw (ex-info "Unknown entry kind" {:kind (first entry-vec)})))

;; ── 主构建函数：支持同符号多条目合并 ──

(defn- combine-entries [existing new-entry]
  (if (= :overloaded (:kind existing))
    (update existing :entries conj new-entry)
    {:kind :overloaded :entries [existing new-entry]}))

(defn build-symbol-table [& entries]
  (reduce (fn [table [sym entry :as _]]
            (if-let [old (get table sym)]
              (assoc table sym (combine-entries old entry))
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

(defn find-entry-by-kind [entry kind-pred]
  (if (= :overloaded (:kind entry))
    (first (filter kind-pred (:entries entry)))   ;; 返回第一个匹配的子条目
    (when (kind-pred entry) entry)))

;; ── 特化：从条目（可能为重载）中提取指定 kind 的单个条目 ──

(defn entry->func      [entry] (find-entry-by-kind entry function-symbol?))
(defn entry->record    [entry] (find-entry-by-kind entry record-symbol?))
(defn entry->protocol  [entry] (find-entry-by-kind entry protocol-symbol?))
(defn entry->variable  [entry] (find-entry-by-kind entry variable-symbol?))
(defn entry->primitive [entry] (find-entry-by-kind entry primitive-symbol?))

;; 任意类型提取（类型包括 :primitive, :record, :protocol）
(defn entry->type      [entry] (find-entry-by-kind entry #(#{:primitive :record :protocol} (:kind %))))

;; ── 类型符号提取（支持重载）──

(defn types-symbols
  [symbol-table]
  (into #{}
        (comp (filter (fn [[_ entry]] (entry->type entry)))
              (map key))
        symbol-table))

;; ── 按种类查找（使用新工具）──

(defn lookup-sym
  "在单个表中查找符号 sym，返回可能为单一条目或重载条目，或 nil。"
  [table sym]
  (get table sym))

(defn lookup-func      [table sym] (entry->func      (lookup-sym table sym)))
(defn lookup-record    [table sym] (entry->record    (lookup-sym table sym)))
(defn lookup-protocol  [table sym] (entry->protocol  (lookup-sym table sym)))
(defn lookup-variable  [table sym] (entry->variable  (lookup-sym table sym)))
(defn lookup-primitive [table sym] (entry->primitive (lookup-sym table sym)))
(defn lookup-type      [table sym] (entry->type      (lookup-sym table sym)))

;; ── 其他辅助函数保持不变 ──

(defn lookup-field-type
  [record-entry field-name]
  (when-let [fields (:fields record-entry)]
    (some (fn [f] (when (= (:field-name f) field-name) (:type f))) fields)))

(defn list-arities
  [func-entry]
  (or (:arities func-entry)
      (when (:params func-entry)
        [(select-keys func-entry [:params :ret])])))

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
  "按顺序在多个符号表中查找 sym，返回第一个找到的条目（可能为重载条目）。"
  [sym & tables]
  (some (fn [tbl] (get tbl sym)) tables))