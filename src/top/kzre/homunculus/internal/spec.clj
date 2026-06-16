(ns top.kzre.homunculus.internal.spec
  "编译器内部数据结构规范"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.core.types.protocol :as tp]))

;; ── 符号表条目 ──
;; 符号，必须是命名空间符号
(s/def ::sym (s/and symbol? (comp some? namespace)))

;; 符号的元数据
(s/def ::meta map?)

;; 条目种类
(s/def ::kind #{:function :record :protocol :variable})

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
;; clojure 不会产生多返回值
(s/def ::function-entry
  (s/merge ::common-entry
           (s/keys :opt-un [::params ::ret ])))

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

;; 单个 arity 签名
(s/def ::method-arity (s/keys :req-un [::params]
                              :opt-un [::ret ::rets]))

;; 一个方法（包含所有重载版本）
(s/def ::method (s/keys :req-un [::method-name ::arities]))
(s/def ::arities (s/coll-of ::method-arity :kind vector? :min-count 1))

;; 协议的方法列表
(s/def ::methods (s/coll-of ::method :kind vector?))

(s/def ::protocol-entry
  (s/merge ::common-entry
           (s/keys :req-un [::methods])))

;; ── 变量特有字段（无额外字段） ──
(s/def ::variable-entry ::common-entry)

;; ── multi-spec 分发 ──
(defmulti symbol-entry-kind :kind)
(defmethod symbol-entry-kind :function [_] ::function-entry)
(defmethod symbol-entry-kind :record   [_] ::record-entry)
(defmethod symbol-entry-kind :protocol [_] ::protocol-entry)
(defmethod symbol-entry-kind :variable [_] ::variable-entry)

(s/def ::symbol-entry (s/multi-spec symbol-entry-kind :kind))

;; ── 完整的导出符号表 ──
(s/def ::exports
  (s/map-of ::sym ::symbol-entry :conform-keys true))

;; ── 编译产物 ──
(s/def ::code string?)
(s/def ::namespace symbol?)
(s/def ::emit-result
  (s/keys :req-un [::code ::exports ::namespace]))

;; ── 可选的附加 spec ──
(s/def ::macro? boolean?)
(s/def ::inline? boolean?)
(s/def ::doc string?)