(ns top.kzre.homunculus.core.ir2.spec
  "IR2 语言无关 AST 的 clojure.spec 定义。"
  (:require [clojure.spec.alpha :as s]))

(s/def ::kind keyword?)
(s/def ::val any?)
(s/def ::name symbol?)
(s/def ::args vector?)
(s/def ::fn any?)                  ;; :call 的函数表达式
(s/def ::test any?)
(s/def ::then any?)
(s/def ::else (s/nilable any?))
(s/def ::exprs vector?)
(s/def ::bindings vector?)        ;; [[var val] ...]
(s/def ::body vector?)
(s/def ::params (s/coll-of ::node :kind vector?))  ;; 参数是 IR2 节点向量
(s/def ::doc (s/nilable string?))
(s/def ::attrs (s/nilable map?))
(s/def ::var any?)                ;; :assign 的左值
(s/def ::catches vector?)
(s/def ::finally (s/nilable vector?))
(s/def ::class symbol?)
(s/def ::sym symbol?)
(s/def ::captures (s/coll-of symbol? :kind vector?))
(s/def ::meta (s/nilable map?))

(s/def ::literal (s/keys :req-un [::kind ::val] :opt-un [::meta]))
(s/def ::variable (s/keys :req-un [::kind ::name] :opt-un [::meta]))
(s/def ::call    (s/keys :req-un [::kind ::fn ::args] :opt-un [::meta]))
(s/def ::if      (s/keys :req-un [::kind ::test ::then] :opt-un [::else ::meta]))
(s/def ::block   (s/keys :req-un [::kind ::exprs] :opt-un [::meta]))
(s/def ::let     (s/keys :req-un [::kind ::bindings ::body] :opt-un [::meta]))
(s/def ::loop    (s/keys :req-un [::kind ::bindings ::body] :opt-un [::meta]))
(s/def ::recur   (s/keys :req-un [::kind ::args] :opt-un [::meta]))
(s/def ::lambda  (s/keys :req-un [::kind ::params ::body ::captures] :opt-un [::name ::meta]))
(s/def ::define  (s/keys :req-un [::kind ::name] :opt-un [::doc ::attrs ::val ::meta]))
(s/def ::vector  (s/keys :req-un [::kind ::items] :opt-un [::meta]))
(s/def ::map     (s/keys :req-un [::kind ::pairs] :opt-un [::meta]))
(s/def ::try     (s/keys :req-un [::kind ::body ::catches] :opt-un [::finally ::meta]))
(s/def ::catch   (s/keys :req-un [::kind ::class ::sym ::body] :opt-un [::meta]))
(s/def ::throw   (s/keys :req-un [::kind ::exprs] :opt-un [::meta]))
(s/def ::assign  (s/keys :req-un [::kind ::var ::val] :opt-un [::meta]))

(defmulti node-spec :kind)
(defmethod node-spec :literal [_] ::literal)
(defmethod node-spec :variable [_] ::variable)
(defmethod node-spec :call    [_] ::call)
(defmethod node-spec :if      [_] ::if)
(defmethod node-spec :block   [_] ::block)
(defmethod node-spec :let     [_] ::let)
(defmethod node-spec :loop    [_] ::loop)
(defmethod node-spec :recur   [_] ::recur)
(defmethod node-spec :lambda  [_] ::lambda)
(defmethod node-spec :define  [_] ::define)
(defmethod node-spec :vector  [_] ::vector)
(defmethod node-spec :map     [_] ::map)
(defmethod node-spec :try     [_] ::try)
(defmethod node-spec :catch   [_] ::catch)
(defmethod node-spec :throw   [_] ::throw)
(defmethod node-spec :assign  [_] ::assign)
(defmethod node-spec :default [node]
  (throw (ex-info "Unknown IR2 node kind" {:kind (:kind node)})))

(s/def ::node (s/multi-spec node-spec :kind))

(declare ir2-vector?)
(s/def ::ir2-vector
  (s/and vector?
         (s/cat :node ::node :children (s/* ::ir2-vector))))

(defn valid-node? [node] (s/valid? ::node node))
(defn explain-node [node] (s/explain ::node node))
(defn valid-ir2? [ir2-vec] (s/valid? ::ir2-vector ir2-vec))