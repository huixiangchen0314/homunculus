(ns top.kzre.homunculus.core.ir1.spec
  "IR1 节点 spec，使用命名空间关键字 ::kind 等，并通过 :req-un / :opt-un 定义。"
  (:require [clojure.spec.alpha :as s]))

;; ── 命名空间限定的属性 spec ────────────────
(s/def ::kind keyword?)
(s/def ::val any?)
(s/def ::name symbol?)
(s/def ::meta (s/nilable map?))
(s/def ::items vector?)
(s/def ::pairs (s/map-of any? any?))
(s/def ::op any?)
(s/def ::args vector?)
(s/def ::test any?)
(s/def ::then any?)
(s/def ::else (s/nilable any?))
(s/def ::exprs vector?)
(s/def ::bindings vector?)
(s/def ::body vector?)
(s/def ::doc (s/nilable string?))
(s/def ::attr (s/nilable map?))
(s/def ::var-sym symbol?)
(s/def ::var symbol?)
(s/def ::catches vector?)
(s/def ::finally (s/nilable vector?))
(s/def ::class symbol?)
(s/def ::sym symbol?)

;; 用于 fn* 参数列表中的元素
(s/def ::param-sym symbol?)
(s/def ::param-meta (s/nilable map?))
(s/def ::fn-param (s/keys :req-un [::param-sym ::param-meta]))
(s/def ::params (s/coll-of ::fn-param :kind vector?))

;; ── 节点 spec，统一使用 :req-un / :opt-un ──
(s/def ::literal (s/keys :req-un [::kind ::val]))
(s/def ::symbol  (s/keys :req-un [::kind ::name] :opt-un [::meta]))
(s/def ::vector  (s/keys :req-un [::kind ::items]))
(s/def ::map     (s/keys :req-un [::kind ::pairs]))
(s/def ::call    (s/keys :req-un [::kind ::op ::args]))
(s/def ::if      (s/keys :req-un [::kind ::test ::then] :opt-un [::else]))
(s/def ::do      (s/keys :req-un [::kind ::exprs]))
(s/def ::let*    (s/keys :req-un [::kind ::bindings ::body]))
(s/def ::fn*     (s/keys :req-un [::kind ::params ::body] :opt-un [::name]))
(s/def ::def     (s/keys :req-un [::kind ::name] :opt-un [::doc ::attr ::val]))
(s/def ::loop    (s/keys :req-un [::kind ::bindings ::body]))
(s/def ::recur   (s/keys :req-un [::kind ::exprs]))
(s/def ::quote   (s/keys :req-un [::kind ::exprs]))  ;; 注意 quote 的 expr 是 any?，沿用 ::exprs 名
(s/def ::var     (s/keys :req-un [::kind ::var-sym]))
(s/def ::throw   (s/keys :req-un [::kind ::exprs]))
(s/def ::set!    (s/keys :req-un [::kind ::var ::val]))
(s/def ::try     (s/keys :req-un [::kind ::body ::catches] :opt-un [::finally]))
(s/def ::catch   (s/keys :req-un [::kind ::class ::sym ::body]))

;; ── 多方法按 ::kind 值选择 spec ──────────────
(defmulti node-spec :kind)
(defmethod node-spec :literal [_] ::literal)
(defmethod node-spec :symbol  [_] ::symbol)
(defmethod node-spec :call    [_] ::call)
(defmethod node-spec :vector  [_] ::vector)
(defmethod node-spec :map     [_] ::map)
(defmethod node-spec :if      [_] ::if)
(defmethod node-spec :do      [_] ::do)
(defmethod node-spec :let*    [_] ::let*)
(defmethod node-spec :fn*     [_] ::fn*)
(defmethod node-spec :def     [_] ::def)
(defmethod node-spec :loop    [_] ::loop)
(defmethod node-spec :recur   [_] ::recur)
(defmethod node-spec :quote   [_] ::quote)
(defmethod node-spec :var     [_] ::var)
(defmethod node-spec :throw   [_] ::throw)
(defmethod node-spec :set!    [_] ::set!)
(defmethod node-spec :try     [_] ::try)
(defmethod node-spec :catch   [_] ::catch)
(defmethod node-spec :default [_] (throw (ex-info "Unknown IR1 node kind" {:kind (::kind _)})))

(s/def ::node (s/multi-spec node-spec ::kind))

;; ── 验证函数 ────────────────────────────────
(defn valid-node? [node] (s/valid? ::node node))
(defn explain-node [node] (s/explain ::node node))

;; ── 递归 IR1 向量验证（子节点也用此 spec）──
(declare ir1-vector?)
(s/def ::ir1-vector
  (s/and vector?
         (s/cat :node ::node
                :children (s/* ::ir1-vector))))

(defn valid-ir1? [ir1-vec] (s/valid? ::ir1-vector ir1-vec))