(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。"
  (:require
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]
    [top.kzre.homunculus.core.ir1.ns-form :as ns-form]
    [top.kzre.homunculus.core.error :as err])
  (:import (top.kzre.homunculus.core.ir1.ns_form NsForm)))

;; ── Env ─────────────────────────────────
(defprotocol IEnv
  (ns-sym [_] "当前命名空间符号")
  (bind-namespace [_ nsf] "绑定 NsForm，重复绑定报错")
  (qualify-symbol [_ sym] "local → 原名；别名 → 解析；其余 → 本 ns 全限定")
  (bind-local-symbol [_ sym] "绑定本地符号")
  (bind-global-symbol [_ sym] "绑定全局符号")
  (local-symbols [_] "当前本地符号集合")
  (global-symbols [_] "当前全局符号集合")
  (with-locals [_ locals] "返回 local-symbols 替换为 locals 的新 env"))

(defn- resolve-alias
  [env sym]
  (when-let [nsf (:ns-form env)]
    (or (first (ns-form/reference-alias-in-ns nsf :require-macros sym))
        (first (ns-form/reference-alias-in-ns nsf :require sym)))))

(defrecord Env
  [compile-ctx ^NsForm ns-form global-symbols local-symbols]
  IEnv
  (ns-sym [_] (when ns-form (ns-form/ns-sym ns-form)))

  (bind-namespace [this nsf]
    (if ns-form
      (throw (err/compile-error
               :multiple-ns-declarations
               "multiple-ns-declarations"
               {:existing ns-form :new nsf}))
      (assoc this :ns-form nsf)))

  (qualify-symbol [this sym]
    (cond
      (contains? local-symbols sym)  sym
      (namespace sym)                sym
      :else
      (or (resolve-alias this sym)
          (if-let [ns (ns-sym this)]
            (symbol (str ns) (name sym))
            sym))))

  (bind-local-symbol [this sym]
    (update this :local-symbols conj sym))

  (bind-global-symbol [this sym]
    (update this :global-symbols conj sym))

  (local-symbols [_] local-symbols)
  (global-symbols [_] global-symbols)

  (with-locals [this locals]
    (assoc this :local-symbols locals)))

(defn make-env
  ([] (make-env nil))
  ([compile-ctx]
   (map->Env {:compile-ctx    compile-ctx
              :ns-form        nil
              :global-symbols #{}
              :local-symbols  #{}})))

;; ── 分派器 ──────────────────────────────
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

;; ── 顶层 def：进 global-symbols ──────────
(defn- lower-def
  [form env]
  (let [[_ sym & more] form
        docstring      (when (string? (first more)) (first more))
        rest-after-doc (if docstring (rest more) more)
        attr-map       (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr       (if attr-map (second rest-after-doc) (first rest-after-doc))
        def-meta       (merge (meta form) (meta sym) (meta val-expr) attr-map)
        env-sym        (bind-global-symbol env sym)
        [val-node env'] (if val-expr
                          (lower-node val-expr env-sym)
                          [nil env-sym])]
    [(n/make-def sym val-node docstring def-meta) env']))

;; ── lower-seq ───────────────────────────
(defn lower-seq
  [forms env]
  (reduce (fn [[acc e] form]
            (let [[node e'] (lower-node form e)]
              [(conj acc node) e']))
          [[] env]
          forms))

(defn lower-node [form env]
  (lower-node* form env))

;; ── 顶层 lower：def 独立处理 ─────────────
(defn- lower-top
  [form env]
  (if (and (seq? form) (= 'def (first form)))
    (lower-def form env)
    (lower-node form env)))

(defn lower
  [forms env]
  (reduce (fn [[acc e] form]
            (let [[node e'] (lower-top form e)]
              [(conj acc node) e']))
          [[] env]
          forms))

;; ── :default ────────────────────────────
(defmethod lower-node* :default [form env]
  (if (seq? form)
    (let [[op & args] form
          [op-node env']    (lower-node op env)
          [arg-nodes env''] (lower-seq args env')]
      [(m/->Call op-node arg-nodes (meta form)) env''])
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))