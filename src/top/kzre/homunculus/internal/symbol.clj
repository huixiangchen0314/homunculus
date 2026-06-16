(ns top.kzre.homunculus.internal.symbol
  "符号表条目构建、访问和判断工具"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.internal.spec :as spec]
            [top.kzre.homunculus.core.types.protocol :as tp]))

;; ── 内部辅助 ──
(defn- validate!
  "验证条目符合 spec，失败则抛出异常"
  [entry]
  (when-not (s/valid? ::spec/symbol-entry entry)
    (throw (ex-info "Invalid symbol entry"
                    {:explain (s/explain-str ::spec/symbol-entry entry)
                     :entry entry})))
  entry)

;; ── 构建函数 ──

(defn make-func
  "构建函数符号条目。
  参数:
  - sym        : 全限定符号 (如 'my.ns/my-func)
  - params     : 参数列表 [{:param-name sym, :type IType?, :meta map?} ...]
  - ret        : 单返回值 {:type IType, :meta map?} 或 nil
  - rets       : 多返回值 [{:ret-name sym, :type IType, :meta map?} ...] 或 nil
  - type       : 函数类型 (IType)，可选
  - meta       : 符号元数据"
  [sym & {:keys [params ret rets type meta]
          :or {params []}}]
  (let [entry {:kind :function
               :sym sym
               :type type
               :meta meta
               :params params
               :ret ret
               :rets rets}]
    (validate! entry)
    entry))

(defn make-record
  "构建记录符号条目。
  参数:
  - sym        : 全限定符号
  - fields     : 字段列表 [{:field-name sym, :type IType?, :meta map?} ...]
  - protocols  : 实现的协议列表 [sym ...] (协议的全限定符号)
  - type       : 记录类型 (IType)，可选
  - meta       : 符号元数据"
  [sym & {:keys [fields protocols type meta]
          :or {fields [] protocols []}}]
  (let [entry {:kind :record
               :sym sym
               :type type
               :meta meta
               :fields fields
               :protocols protocols}]
    (validate! entry)
    entry))

(defn make-protocol
  "构建协议符号条目。
  参数:
  - sym         : 全限定符号
  - methods     : 方法列表 [{:method-name sym
                              :arities [{:params [...]
                                         :ret {...}? :rets [...]?} ...]} ...]
  - type        : 协议类型 (IType)，可选
  - meta        : 符号元数据"
  [sym & {:keys [methods type meta]
          :or {methods []}}]
  (let [entry {:kind :protocol
               :sym sym
               :type type
               :meta meta
               :methods methods}]
    (validate! entry)
    entry))

(defn make-variable
  "构建变量符号条目。
  参数:
  - sym        : 全限定符号
  - type       : 变量类型 (IType)，可选
  - meta       : 符号元数据"
  [sym & {:keys [type meta]}]
  (let [entry {:kind :variable
               :sym sym
               :type type
               :meta meta}]
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

(defn make-method-arity
  "构建方法的一个 arity 签名"
  [params & {:keys [ret rets]}]
  (cond-> {:params params}
          ret (assoc :ret ret)
          rets (assoc :rets rets)))

(defn make-method
  "构建一个方法（可能包含多个 arity）"
  [name arities]
  {:method-name name :arities (vec arities)})

;; ── 访问函数 ──

(defn get-kind
  "获取条目种类"
  [entry]
  (:kind entry))

(defn get-type
  "获取条目的类型（可能为 nil）"
  [entry]
  (:type entry))

(defn get-meta
  "获取条目的元数据"
  [entry]
  (:meta entry))

(defn get-fields
  "获取记录的字段列表（仅 record 有效）"
  [entry]
  (:fields entry))

(defn get-protocols
  "获取记录实现的协议列表（仅 record 有效）"
  [entry]
  (:protocols entry))

(defn get-methods
  "获取协议的方法列表（仅 protocol 有效）"
  [entry]
  (:methods entry))

(defn get-params
  "获取函数的参数列表（仅 function 有效）"
  [entry]
  (:params entry))

(defn get-ret
  "获取函数的单返回值（可能为 nil）"
  [entry]
  (:ret entry))

(defn get-rets
  "获取函数的多返回值列表（可能为 nil）"
  [entry]
  (:rets entry))

;; ── 判断函数 ──

(defn function?
  "判断条目是否为函数"
  [entry]
  (= (:kind entry) :function))

(defn record?
  "判断条目是否为记录"
  [entry]
  (= (:kind entry) :record))

(defn protocol?
  "判断条目是否为协议"
  [entry]
  (= (:kind entry) :protocol))

(defn variable?
  "判断条目是否为变量"
  [entry]
  (= (:kind entry) :variable))