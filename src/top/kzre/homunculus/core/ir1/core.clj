(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。"
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir1.ast :as m]
    [top.kzre.homunculus.core.ir1.node :as n]
    [clojure.string :as str]
    [top.kzre.homunculus.core.ir1.ns-form :as ns-form]
    [top.kzre.homunculus.core.ir1.reference :as reference]
    [top.kzre.homunculus.internal.protocol :as ip])
  (:import
    (top.kzre.homunculus.core.ir1.ns_form NsForm)))

;; ── Env ─────────────────────────────────
(defprotocol IEnv
  (ns-sym [_] "当前命名空间符号")
  (bind-namespace [_ nsf] "绑定 NsForm，重复绑定报错")
  (qualify-ns-symbol [_ sym] "定义目标：本 ns 全限定；带前缀报错")
  (qualify-symbol [_ sym] "引用：local → 原名；别名 → 解析；其余 → 本 ns 全限定")
  (bind-local-symbol [_ sym] "绑定本地符号")
  (bind-global-symbol [_ sym] "绑定全局符号")
  (local-symbols [_] "当前本地符号集合")
  (global-symbols [_] "当前全局符号集合")
  (with-locals [_ locals] "返回 local-symbols 替换为 locals 的新 env"))

(defn- qualify-macro-symbol
  "宏空间：候选可能来自 :refer :all，逐个 resolve 验证，返回第一个能 resolve 的。"
  [nsf sym]
  (when nsf
    (some (fn [candidate]
            (when (resolve candidate) candidate))
          (ns-form/reference-alias-in-ns nsf :require-macros sym))))

(defn- qualify-normal-symbol
  "非宏空间：expand-refer-all 已展开为显式 :refer [x y]，符号一定存在，first 即可。"
  [nsf sym]
  (when nsf
    (first (ns-form/reference-alias-in-ns nsf :require sym))))

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
      (let [;; 1. cljh.core 注入 :require——shader 标准库
            nsf1 (if (and compile-ctx
                          (not= (ns-form/ns-sym nsf) 'cljh.core))
                   (ns-form/prepend-reference nsf :require ['cljh.core :refer :all])
                   nsf)
            ;; 2. frontend 声明的宏空间注入 :require-macros——内置 DSL 宏
            macro-nss (when compile-ctx
                        (seq (ip/macro-namespaces compile-ctx)))
            nsf2 (if macro-nss
                   (apply ns-form/prepend-reference nsf1 :require-macros
                          (mapv (fn [ns*] [ns* :refer :all]) macro-nss))
                   nsf1)]
        (assoc this :ns-form (reference/resolve-ns! compile-ctx nsf2)))))

  (qualify-ns-symbol [this sym]
    (when (qualified-symbol? sym)
      (throw (err/compile-error
               :symbol-with-invalid-qualified
               "定义目标符号不允许带命名空间前缀"
               {:sym sym})))
    (if-let [ns (ns-sym this)]
      (symbol (str ns) (name sym))
      sym))

  (qualify-symbol [this sym]
    (cond
      (contains? local-symbols sym)  sym

      (ip/builtin-instruction? compile-ctx sym)
      sym

      :else
      (or (qualify-macro-symbol ns-form sym)      ; 宏空间：resolve 验证
          (qualify-normal-symbol ns-form sym)     ; 非宏空间：expand-refer-all 已展开
          (if (namespace sym)
            sym                                    ; 已带前缀，原样返回
            (let [ns   (ns-sym this)
                  qsym (when ns (symbol (str ns) (name sym)))]
              (if (and qsym (contains? global-symbols qsym))
                qsym                               ; 本模块定义过的全局符号 → 加前缀
                sym))))))                          ; 否则：字段名 / 其他来源 → 原样

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

;; ── 顶层 def：定义目标 qualify，进 global-symbols ──
(defn- lower-def
  [form env]
  (let [[_ sym & more] form
        docstring      (when (string? (first more)) (first more))
        rest-after-doc (if docstring (rest more) more)
        attr-map       (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr       (if attr-map (second rest-after-doc) (first rest-after-doc))
        def-meta       (merge (meta form) (meta sym) (meta val-expr) attr-map)
        ;; 定义目标 qualify 到本 ns，保留原 meta（^:inline / 行号 等）
        sym'           (with-meta (qualify-ns-symbol env sym) (meta sym))
        env-sym        (bind-global-symbol env sym')
        [val-node env'] (if val-expr
                          (lower-node val-expr env-sym)
                          [nil env-sym])]
    [(n/make-def sym' val-node docstring def-meta) env']))

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

(declare lower-top lower-seq-top)

(defn- lower-seq-top
  "对一串顶层表单调用 lower-top，返回 [nodes env']。
   env 沿表单串行传播——前一个顶层 def 的符号对后一个可见。"
  [forms env]
  (reduce (fn [[acc e] form]
            (let [[node e'] (lower-top form e)]
              [(conj acc node) e']))
          [[] env]
          forms))

(defn- lower-top
  "顶层 lower：处理顶层 def 和顶层 do（子表达式仍在顶层）。"
  [form env]
  (cond
    (and (seq? form) (= 'def (first form)))
    (lower-def form env)

    (and (seq? form) (= 'do (first form)))
    (let [[_ & exprs] form
          [nodes env'] (lower-seq-top exprs env)]
      [(m/->Do nodes (meta form)) env'])

    :else
    (lower-node form env)))

(defn lower
  "lower 一系列顶层表单，返回 {:asts ... :ns-sym ...}。"
  [forms env]
  (let [[asts env'] (lower-seq-top forms env)]
    {:asts   asts
     :ns-sym (ns-sym env')}))

(defn- expand-macro
  "尝试宏展开。返回展开后的 form，或 nil（非宏 / 展开无进展）。

   利用 env 的 qualify-symbol 解析 op，得到全限定符号后判断是否是宏。
   cljh 的宏已在 reference/resolve-ns! 阶段通过 load-macro-ns! 加载到 JVM，
   resolve 能拿到 var。"
  [form env]
  (when (and (seq? form) (symbol? (first form)))
    (let [op             (first form)
          qualified-op   (qualify-symbol env op)]
      (when (namespace qualified-op)
        (let [qualified-form (with-meta (cons qualified-op (rest form)) (meta form))
              v              (resolve qualified-op)]
          (when (and v (:macro (meta v)))
            (let [expanded (macroexpand-1 qualified-form)]
              (when-not (identical? expanded qualified-form)
                expanded))))))))

;; ── :default ────────────────────────────
(defmethod lower-node* :default [form env]
  (if (seq? form)
    (if-let [expanded (expand-macro form env)]
      ;; 宏展开后走 lower-top——和源码位置一致地处理 def / 特殊形式
      (lower-top expanded env)
      ;; 非宏：普通调用
      (let [[op & args] form
            [op-node env']    (lower-node op env)
            [arg-nodes env''] (lower-seq args env')]
        [(m/->Call op-node arg-nodes (meta form)) env'']))
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))