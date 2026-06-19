(ns top.kzre.homunculus.internal.symbol
  "符号表条目构建、访问和判断工具。提供类似 Hiccup 的 DSL 来构建符号表。"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.internal.spec :as spec]))

;; ========== 构建与验证（前部分保持不变） ==========


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
    (keyword? type-spec) (ty/make-tcon (symbol (name type-spec)))  ; :float -> TCon 'float
    (symbol? type-spec)  (ty/make-tcon type-spec)                  ; 'float -> TCon 'float
    (vector? type-spec)  ;; 保持原有复杂类型处理
    (let [parts (partition-by #{'->} type-spec)
          types (remove #{['->]} parts)]
      (reduce (fn [ret arg]
                (ty/make-tfun (parse-type (first arg)) ret))
              (parse-type (last (last types)))
              (reverse (butlast types))))
    :else type-spec))

;; ── 构建函数 ──

(defn make-func
  "构建函数符号条目。
   支持两种模式：
   1. 重载模式（优先）：通过 :arities 提供多重重载列表
      :arities 为向量，每个元素为 {:params [...] :ret {...}?}
   2. 简单模式：通过 :params / :ret / :rets 提供单一签名
   其他可选参数：:type, :meta"
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
  "构建记录符号条目。
   参数:
   - sym        : 全限定符号
   - fields     : 字段列表 [{:field-name sym, :type IType?, :meta map?} ...]
   - protocols  : 实现的协议列表 [sym ...]
   - type       : 记录类型 (IType)，可选
   - meta       : 符号元数据"
  [sym & {:keys [fields protocols type meta] :or {fields [] protocols []}}]
  (let [entry (cond-> {:kind :record :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta)
                      fields (assoc :fields fields)
                      protocols (assoc :protocols protocols))]
    (validate! entry)
    entry))

(defn make-protocol
  "构建协议符号条目。
   参数:
   - sym         : 全限定符号
   - methods     : 方法列表 [{:method-name sym
                              :arities [{:params [...]
                                         :ret {...}?} ...]} ...]
   - type        : 协议类型 (IType)，可选
   - meta        : 符号元数据"
  [sym & {:keys [methods type meta] :or {methods []}}]
  (let [entry (cond-> {:kind :protocol :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta)
                      methods (assoc :methods methods))]
    (validate! entry)
    entry))
(defn make-variable
  "构建变量符号条目。
   参数:
   - sym        : 全限定符号
   - type       : 变量类型 (IType)，可选
   - meta       : 符号元数据"
  [sym & {:keys [type meta]}]
  (let [entry (cond-> {:kind :variable :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta))]
    (validate! entry)
    entry))

;; ── 便捷辅助构建函数 ──

(defn make-param
  "构建参数 map"
  [name & {:keys [type meta]}]
  (cond-> {:param-name name}
          type (assoc :type type)
          meta (assoc :meta meta)))

(defn make-field
  "构建字段 map"
  [name & {:keys [type meta]}]
  (cond-> {:field-name name}
          type (assoc :type type)
          meta (assoc :meta meta)))

(defn make-ret
  "构建单返回值 map"
  [type & {:keys [meta]}]
  (cond-> {:type type}
          meta (assoc :meta meta)))

(defn make-ret-item
  "构建多返回值项 map"
  [name type & {:keys [meta]}]
  (cond-> {:ret-name name :type type}
          meta (assoc :meta meta)))

(defn make-func-arity
  "构建一个函数/方法签名（参数列表 + 可选返回类型）"
  [params & {:keys [ret]}]
  (cond-> {:params params}
          ret (assoc :ret ret)))

(defn make-method
  "构建一个协议方法（包含多个 arity）"
  [name arities]
  {:method-name name :arities (vec arities)})

;; 向后兼容别名
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

(defn get-arities
  "获取函数条目的重载列表（vec of func-arity）"
  [entry]
  (:arities entry))

;; 原始类型构造器
(defn make-primitive
  "构建原始类型符号条目。sym 为类型名符号，type 可选（默认 = TCon sym）。"
  [sym & {:keys [type meta]}]
  (let [entry (cond-> {:kind :primitive :sym sym}
                      type (assoc :type type)
                      meta (assoc :meta meta))]
    (validate! entry)
    entry))

;; ── 类似 Hiccup 的 DSL ──
;; ── 多方法：解析符号表项 ──
(defmulti parse-table-entry
          "根据表项的 `kind` 分派，返回键值对序列 [[sym entry] ...]。"
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
        ;; 构造器条目
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

(defmethod parse-table-entry :default
  [entry-vec]
  (throw (ex-info "Unknown entry kind" {:kind (first entry-vec)})))

(defmethod parse-table-entry :primitive
  [[_ sym & rest]]
  (let [sym (unquote-name sym)
        ;; 可选的类型参数，默认类型即为 TCon sym
        type (if (seq rest)
               (parse-type (first rest))
               (ty/make-tcon sym))]
    (list [sym (make-primitive sym :type type)])))

;; ── 主构建函数 ──
(defn build-symbol-table [& entries]
  (into {} (mapcat parse-table-entry entries)))





;; ============================================================
;; 查询工具
;; ============================================================

;; ── 判断函数 ──

(defn function-symbol? [entry] (= (:kind entry) :function))
(defn record-symbol?   [entry] (= (:kind entry) :record))
(defn protocol-symbol? [entry] (= (:kind entry) :protocol))
(defn variable-symbol? [entry] (= (:kind entry) :variable))
(defn primitive-symbol? [entry] (= (:kind entry) :primitive))


(defn types-symbols
  "从符号表中提取所有类型符号（原始类型、记录、协议）。
   返回符号集合，可用于已知类型检查。"
  [symbol-table]
  (into #{}
        (comp (filter (fn [[_ entry]]
                        (#{:primitive :record :protocol} (:kind entry))))
              (map key))
        symbol-table))


(defn lookup-sym
  "在符号表（map 或通过 ICompileContext）中查找符号 sym。
   返回符号条目，或 nil。"
  [table sym]
  (get table sym))

(defn lookup-func
  "查找函数条目，若条目存在且为 :function 则返回，否则返回 nil。"
  [table sym]
  (when-let [entry (lookup-sym table sym)]
    (when (function-symbol? entry) entry)))

(defn lookup-record
  "查找记录条目，若条目存在且为 :record 则返回，否则返回 nil。"
  [table sym]
  (when-let [entry (lookup-sym table sym)]
    (when (record-symbol? entry) entry)))

(defn lookup-protocol
  "查找协议条目，若条目存在且为 :protocol 则返回，否则返回 nil。"
  [table sym]
  (when-let [entry (lookup-sym table sym)]
    (when (protocol-symbol? entry) entry)))

(defn lookup-variable
  "查找变量条目，若条目存在且为 :variable 则返回，否则返回 nil。"
  [table sym]
  (when-let [entry (lookup-sym table sym)]
    (when (variable-symbol? entry) entry)))

(defn lookup-field-type
  "在记录条目中查找字段 field-name 的类型，返回 IType 或 nil。"
  [record-entry field-name]
  (when-let [fields (:fields record-entry)]
    (some (fn [f] (when (= (:field-name f) field-name) (:type f))) fields)))

(defn list-arities
  "返回函数条目的所有重载签名（向量）。若条目只有单签名，也包装为向量返回。"
  [func-entry]
  (or (:arities func-entry)
      (when (:params func-entry)
        [(select-keys func-entry [:params :ret])])))

(defn find-matching-arities
  "根据参数类型列表 arg-tys（IType 序列）查找函数条目的匹配重载。
   匹配条件：参数个数相等，且每个参数类型相等（或通过 type-conversion 存在转换？此处只做精确匹配）。
   返回匹配的 arity（包含 :params 和 :ret），若有多个匹配，返回第一个；无匹配返回 nil。"
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
  "在符号表中查找函数 sym 并尝试匹配 arg-tys，返回匹配的 arity 的返回值类型，或 nil。"
  [table sym arg-tys]
  (when-let [func-entry (lookup-func table sym)]
    (when-let [arity (find-matching-arities func-entry arg-tys)]
      (when-let [ret (:ret arity)]
        (:type ret)))))


(defn lookup-in-tables
  "按顺序在多个符号表中查找 sym，返回第一个找到的条目。
   每个参数应该是一个 map（键为符号，值为符号条目）。
   若所有表中均未找到，返回 nil。"
  [sym & tables]
  (some (fn [tbl] (get tbl sym)) tables))
