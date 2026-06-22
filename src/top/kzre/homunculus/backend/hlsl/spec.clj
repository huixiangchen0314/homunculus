;; ── spec.clj ─────────────────────────────────────────────
(ns top.kzre.homunculus.backend.hlsl.spec
  "HLSL 渲染器输入数据的结构规范 (clojure.spec)。
   无标签向量表示顺序语句序列，极大简化 AST 结构。"
  (:require [clojure.spec.alpha :as s]))

(defn- tag-dispatch [node]
  (if (vector? node) (first node) node))

(defmulti node-tag (fn [node] (tag-dispatch node)))

;; ── 所有带标签节点的 spec ──
(defmethod node-tag :raw [_]
  (s/cat :tag #{:raw} :content any?))

(defmethod node-tag :literal [_]
  (s/cat :tag #{:literal} :val any?))

(defmethod node-tag :var-ref [_]
  (s/cat :tag #{:var-ref} :name (s/or :sym symbol? :str string?)))

(defmethod node-tag :call [_]
  (s/cat :tag #{:call} :fn-name (s/or :sym symbol? :str string?)
         :args (s/* ::node)))

(defmethod node-tag :sample [_]
  (s/cat :tag #{:sample} :tex ::node :sampler ::node :uv ::node))

(defmethod node-tag :binary [_]
  (s/cat :tag #{:binary} :op string? :left ::node :right ::node))

(defmethod node-tag :member-access [_]
  (s/cat :tag #{:member-access} :target ::node :member (s/or :sym symbol? :str string?)))

(defmethod node-tag :constructor [_]
  (s/cat :tag #{:constructor} :type-name string? :args (s/* ::node)))

(defmethod node-tag :cast [_]
  (s/cat :tag #{:cast} :type-str string? :expr ::node))

(defmethod node-tag :return [_]
  (s/cat :tag #{:return} :expr ::node))

(defmethod node-tag :var-decl [_]
  (s/cat :tag #{:var-decl} :type string? :name (s/or :sym symbol? :str string?) :init ::node))

(defmethod node-tag :array-decl [_]
  (s/cat :tag #{:array-decl}
         :elem-type string?
         :name (s/or :sym symbol? :str string?)
         :size ::node))

(defmethod node-tag :assign [_]
  (s/cat :tag #{:assign} :target ::node :value ::node))

(defmethod node-tag :expr-stmt [_]
  (s/cat :tag #{:expr-stmt} :expr ::node))

(defmethod node-tag :if [_]
  (s/cat :tag #{:if} :test ::node :then ::node :else (s/? ::node)))

(defmethod node-tag :while [_]
  (s/cat :tag #{:while} :test ::node :body ::node))

(defmethod node-tag :block [_]
  (s/cat :tag #{:block} :stmts (s/* ::node)))

(defmethod node-tag :function [_]
  (s/cat :tag #{:function} :return-type string? :name (s/or :sym symbol? :str string?)
         :params (s/coll-of (s/tuple string? (s/or :sym symbol? :str string?)))
         :body (s/or :string string? :stmts (s/coll-of ::node))))

(defmethod node-tag :entry-wrapper [_]
  (s/cat :tag #{:entry-wrapper} :stage #{:vertex :fragment}
         :func-name (s/or :sym symbol? :str string?)
         :input-type string? :output-type string? :return-type string?
         :body (s/or :string string? :stmts (s/coll-of ::node))))

(defmethod node-tag :import [_]
  (s/cat :tag #{:import} :path string?))

(defmethod node-tag :struct-member [_]
  (s/cat :tag #{:struct-member} :type string? :name (s/or :sym symbol? :str string?)
         :semantic (s/? (s/nilable string?))))

(defmethod node-tag :struct [_]
  (s/cat :tag #{:struct} :name string? :members (s/coll-of ::node)))

(defmethod node-tag :texture-decl [_]
  (s/cat :tag #{:texture-decl} :name string? :register string?))

(defmethod node-tag :sampler-decl [_]
  (s/cat :tag #{:sampler-decl} :name string? :register string?))

(defmethod node-tag :cbuffer-decl [_]
  (s/cat :tag #{:cbuffer-decl} :name string? :register string?
         :members (s/coll-of ::node)))

(defmethod node-tag :uniform-decl [_]
  (s/cat :tag #{:uniform-decl} :type string? :name (s/or :sym symbol? :str string?)))

(defmethod node-tag :static-var-decl [_]
  (s/cat :tag #{:static-var-decl} :type string? :name (s/or :sym symbol? :str string?) :init ::node))

(defmethod node-tag :comment [_]
  (s/cat :tag #{:comment} :text string?))

(defmethod node-tag :new-array [_]
  (s/cat :tag #{:new-array} :size ::node))

(defmethod node-tag :aget [_]
  (s/cat :tag #{:aget} :target ::node :idx ::node))

(defmethod node-tag :aset [_]
  (s/cat :tag #{:aset} :target ::node :idx ::node :val ::node))

(defmethod node-tag :alength [_]
  (s/cat :tag #{:alength} :target ::node))

;; ★ 关键：无标签向量，表示顺序语句序列
(defmethod node-tag :default [_]
  (s/cat :stmts (s/* ::node)))

;; ── 汇总定义 ──
(s/def ::tagged-node (s/multi-spec node-tag tag-dispatch))
(s/def ::node (s/or :string string? :tagged ::tagged-node))
(s/def ::ast (s/coll-of ::node))

(defn valid-ast? [nodes]
  (s/valid? ::ast (if (vector? nodes) nodes (vec nodes))))

(defn explain-ast [nodes]
  (s/explain-str ::ast (if (vector? nodes) nodes (vec nodes))))