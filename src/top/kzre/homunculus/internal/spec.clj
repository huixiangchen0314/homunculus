(ns top.kzre.homunculus.internal.spec
  "编译器内部数据结构规范"
  (:require [clojure.spec.alpha :as s]))

;; ── 符号表条目 ──
;; 符号可以是命名空间符号或简单符号（用于内置函数/类型）
(s/def ::sym (s/and symbol? #(or (namespace %) true)))

;; 符号的元数据
(s/def ::meta map?)

;; 条目种类
(s/def ::kind #{:function :record :protocol :variable :primitive})

;; ── 通用字段（所有条目共有） ──
(s/def ::common-entry
  (s/keys :req-un [::sym ::kind]
          :opt-un [::type ::meta]))

;; ── 函数特有字段 ──
(s/def ::param-name symbol?)
(s/def ::param (s/keys :req-un [::param-name]
                       :opt-un [::meta ::type]))
(s/def ::params (s/coll-of ::param :kind vector?))
(s/def ::ret (s/keys :opt-un [::meta ::type]))

;; 单个函数/方法的元数签名（参数列表 + 返回类型）
(s/def ::func-arity (s/keys :req-un [::params]
                            :opt-un [::ret]))

;; 函数条目：支持多重重载 (arities) 或简单单重载 (params + ret)
(s/def ::function-entry
  (s/merge ::common-entry
           (s/keys :opt-un [::params ::ret ::arities])))

;; ── 记录特有字段 ──
(s/def ::field-name symbol?)
(s/def ::field (s/keys :req-un [::field-name]
                       :opt-un [::meta ::type]))
(s/def ::fields (s/coll-of ::field :kind vector?))
(s/def ::protocol ::sym)
(s/def ::protocols (s/coll-of ::protocol :kind vector?))

(s/def ::record-entry
  (s/merge ::common-entry
           (s/keys :opt-un [::fields ::protocols])))

;; ── 协议特有字段（支持重载） ──
(s/def ::method-name symbol?)
(s/def ::method (s/keys :req-un [::method-name ::arities]))
(s/def ::arities (s/coll-of ::func-arity :kind vector? :min-count 1))
(s/def ::methods (s/coll-of ::method :kind vector?))

(s/def ::protocol-entry
  (s/merge ::common-entry
           (s/keys :req-un [::methods])))

;; ── 变量特有字段（无额外字段） ──
(s/def ::variable-entry ::common-entry)

;; 原始类型
(s/def ::primitive-entry ::common-entry)


;; ── multi-spec 分发 ──
(defmulti symbol-entry-kind :kind)
(defmethod symbol-entry-kind :function [_] ::function-entry)
(defmethod symbol-entry-kind :record   [_] ::record-entry)
(defmethod symbol-entry-kind :protocol [_] ::protocol-entry)
(defmethod symbol-entry-kind :variable [_] ::variable-entry)
(defmethod symbol-entry-kind :primitive [_] ::primitive-entry)

(s/def ::symbol-entry (s/multi-spec symbol-entry-kind :kind))

;; ── 完整的导出符号表 ──
(s/def ::symbol-table
  (s/map-of ::sym ::symbol-entry :conform-keys true))