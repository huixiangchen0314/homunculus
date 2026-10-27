(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。
   所有特殊形式的解析逻辑在 ir1.forms 中。"
  (:require
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]
    [top.kzre.homunculus.core.ir1.ns-form :as ns-form])
  (:import (top.kzre.homunculus.core.ir1.ns_form NsForm)))

;; ── Env：环境协议与记录 ──────────────────
(defprotocol IEnv
  (ns-sym [_] "返回当前解析出的命名空间"))

(defrecord Env
  [compile-ctx ^NsForm ns-form global-symbols local-symbols]
  IEnv
  (ns-sym [_] (when ns-form (ns-form/ns-sym ns-form))))

(defn make-env
  ([] (make-env nil))
  ([compile-ctx]
   (map->Env {:compile-ctx    compile-ctx
              :ns-form        nil
              :global-symbols #{}
              :local-symbols  #{}})))

;; ── 表单 → 节点记录 分派器 ──────────────
(declare lower-node)

(defmulti lower-node*
          (fn [form _env]
            (cond
              (or (number? form) (string? form) (true? form) (false? form)
                  (nil? form) (keyword? form) (char? form)) :literal
              (symbol? form) :symbol
              (vector? form) :vector
              (map? form)    :map
              (seq? form)    (let [op (first form)]
                               (cond (symbol? op)  op
                                     (keyword? op) :keyword-access
                                     :else         :call))
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))


(defn- lower-def
  [form env]
  (let [[_ sym & more] form
        docstring      (when (string? (first more)) (first more))
        rest-after-doc (if docstring (rest more) more)
        attr-map       (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr       (if attr-map (second rest-after-doc) (first rest-after-doc))
        def-meta       (merge (meta form) (meta sym) (meta val-expr) attr-map)
        [val-node env'] (if val-expr
                          (lower-node val-expr env)
                          [nil env])]
    [(n/make-def sym val-node docstring def-meta) env']))


(defn lower-seq
  "按顺序 lower 一组表单，env 沿每个表单串行积累。
   返回 [nodes env']，nodes 是 IR1 节点向量。"
  [forms env]
  (reduce (fn [[acc e] form]
            (let [[node e'] (lower-node form e)]
              [(conj acc node) e']))
          [[] env]
          forms))


;; ── :default：非特殊形式的调用 ──────────
;; 用 reduce 沿参数积累 env，这就是「环境沿子节点传播」的结构骨架。
(defmethod lower-node* :default [form env]
  (if (seq? form)
    (let [[op & args] form
          [op-node env']    (lower-node op env)
          [arg-nodes env''] (lower-seq args env')]
      [(m/->Call op-node arg-nodes (meta form)) env''])
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))

(defn lower-node [form env]
  (lower-node* form env))

(defn- lower-global [form env]
  (if (and  (seq? form)
            (= 'def (first form)))
    (lower-def form env)
    (lower-node form env)))

(defn lower [forms env]
  (reduce (fn [[acc e] form]
            (let [[node e'] (lower-global form e)]
              [(conj acc node) e']))
          [[] env]
          forms))