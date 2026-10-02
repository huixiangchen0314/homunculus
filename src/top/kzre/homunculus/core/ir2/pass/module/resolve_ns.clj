(ns top.kzre.homunculus.core.ir2.pass.module.resolve-ns
  "命名空间解析。利用 reduce-children 自动遍历，只保留需要环境/名称特殊处理的节点。"
  (:require
   [clojure.walk :as walk]
   [top.kzre.homunculus.core.error :as err]
   [top.kzre.homunculus.core.ir2.ast :as ir2]
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.ir2.pass.namespace :as namespace]
   [top.kzre.homunculus.core.ir2.pass.protocol :as types]
   [top.kzre.homunculus.internal.protocol :as ip]))

(defn- bind-var [env var-names]
  (update env :bound-vars into var-names))

(defn- add-global-def [env def-name]
  (update env :global-defs conj def-name))

;; ── 多方法分派 ──
(defmulti resolve-node*
          (fn [node env] (n/kind node)))

(defn- resolve-symbol [var-name env]
  (cond
    (namespace var-name)
    (if-let [full-ns (get (:aliases env) (symbol (namespace var-name)))]
      (symbol (str full-ns) (name var-name))
      var-name)
    (contains? (:bound-vars env) var-name) var-name
    (contains? (:aliases env) var-name)
    (get (:aliases env) var-name)
    (contains? (:global-defs env) var-name)
    (symbol (str (:ns-name env)) (name var-name))
    :else var-name))

(defn- resolve-map-symbols
  "递归遍历元数据结构，对所有符号应用 resolve-symbol。
   符号以外的值（字符串、数字、关键字等）保持原样。"
  [x env]
  (walk/postwalk
    (fn [v] (if (symbol? v) (resolve-symbol v env) v))
    x))

(defn- resolve-meta
  "处理节点元数据中的所有符号引用。
   遍历整个 meta map（含嵌套结构），对每个符号应用 resolve-symbol。"
  [node env]
  (if-let [md (n/node-meta node)]
    (assoc node :meta (resolve-map-symbols md env))
    node))


(defn resolve-node
  "对单个节点做名称解析：先处理节点主体，再处理其 meta 中的符号。"
  [node env]
  (let [[new-node env'] (resolve-node* node env)]
    [(resolve-meta new-node env') env']))

(defn walk [node env]
  (ir2/reduce-children node
                       (fn [child current-env]
                         (resolve-node child current-env))
                       env))

;; ★ ns：不做变换
(defmethod resolve-node* :ns [node env]
  [node env])

;; ★ define：正规化名称 + 注册全局定义，然后递归值表达式
(defmethod resolve-node* :define [node env]
  (let [old-name (n/define-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:ns-name env)) (name old-name)))
        unqualified-name (symbol (name old-name))
        env'     (add-global-def env unqualified-name)
        val-node (n/define-val node)
        [new-val env''] (if val-node (resolve-node val-node env') [nil env'])]
    [(n/make-define new-name new-val
                    (n/define-docstring node)
                    (n/attrs node)
                    (n/node-meta node))
     env'']))

;; ★ lambda：创建新作用域，参数加入局部变量，不替换参数
(defmethod resolve-node* :lambda [node env]
  (let [params (n/lambda-params node)
        param-names (map n/var-name params)
        env' (bind-var env param-names)
        [new-body _] (resolve-node (n/lambda-body node) env')]
    [(n/make-lambda params new-body
                    (n/lambda-captures node)
                    (n/lambda-fn-name node)
                    (n/attrs node)
                    (n/node-meta node))
     env]))

;; ★ let：创建新作用域，绑定变量加入局部变量，顺序处理值表达式和体
(defmethod resolve-node* :let [node env]
  (let [bindings (n/let-bindings node)
        binding-names (set (map #(:name (:var %)) bindings))
        env-inner (bind-var env binding-names)
        ;; 顺序处理每个 Binding 的值表达式，保留变量不处理
        [new-bindings env']
        (reduce (fn [[bnds e] b]
                  (let [var-node (:var b)
                        val-node (:val b)
                        [new-val e2] (resolve-node val-node e)
                        new-binding (assoc b :var var-node :val new-val)]
                    [(conj bnds new-binding) e2]))
                [[] env]
                bindings)
        [new-body _] (resolve-node (n/let-body node) env-inner)]
    [(ir2/->Let new-bindings new-body (:attrs node) (:meta node))
     env]))

;; loop：类似 let，创建新作用域
(defmethod resolve-node* :loop [node env]
  (let [bindings (n/loop-bindings node)
        binding-names (set (map #(:name (:var %)) bindings))
        env-inner (bind-var env binding-names)
        [new-bindings _env'] (reduce (fn [[bnds e] b]
                                       (let [expr      (:val b)
                                             [new-expr e2] (resolve-node expr e)
                                             new-b     (assoc b :val new-expr)]
                                         [(conj bnds new-b) e2]))
                                     [[] env]
                                     bindings)
        [new-body _] (resolve-node (n/loop-body node) env-inner)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node))
     env]))


;; ★ variable：根据作用域规则决定是否加上命名空间前缀
(defmethod resolve-node* :variable [node env]
  (let [var-name (n/var-name node)
        new-node (n/variable-with-name node (resolve-symbol var-name env))]
    [new-node env]))

(defmethod resolve-node* :catch [node env]
  (let [sym       (n/catch-sym node)
        sym-name  (n/var-name sym)
        inner-env (bind-var env [sym-name])
        body      (n/catch-body node)
        [new-body _] (reduce (fn [[bs _] expr]
                               (let [[new-expr _] (resolve-node expr inner-env)]
                                 [(conj bs new-expr) inner-env]))
                             [[] inner-env]
                             body)]
    [(n/make-catch (n/catch-class node) sym new-body
                   (n/attrs node) (n/node-meta node))
     env]))

;; ★ record / protocol：名称正规化，其余子节点由 reduce-children 自动处理（见默认方法）
(defmethod resolve-node* :record [node env]
  (let [old-name (n/record-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:ns-name env)) (name old-name)))
        unqualified-name (symbol (name old-name))
        env'     (add-global-def env unqualified-name)
        [new-node env''] (walk (n/record-with-name node new-name) env')]
    [new-node env'']))

(defmethod resolve-node* :protocol [node env]
  (let [old-name (n/protocol-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:ns-name env)) (name old-name)))
        unqualified-name (symbol (name old-name))
        env'     (add-global-def env unqualified-name)
        [new-node env''] (walk (n/protocol-with-name node new-name) env')]
    [new-node env'']))

;; ★ 默认方法：利用 reduce-children 自动遍历并重建节点
(defmethod resolve-node* :default [node env]
  (walk node env))



;; ── 主函数不变 ──
(defn resolve-ns
  [ir2-roots compile-ctx]
  (let [frontend     (ip/frontend compile-ctx)
        ns-nodes     (filter #(= (ir2/kind %) :ns) ir2-roots)
        non-ns-roots (remove #(= (ir2/kind %) :ns) ir2-roots)
        ns-count     (count ns-nodes)
        _            (cond
                       (zero? ns-count)
                       (throw (err/compile-error
                                :missing-ns-declaration
                                "No namespace declaration found"
                                {}))

                       (> ns-count 1)
                       (throw (err/compile-error
                                :multiple-ns-declarations
                                (str "Multiple namespace declarations found: " ns-count)
                                {:count ns-count})))
        self-ns       (some-> ns-nodes first :name)
        macro-ns      (when frontend (types/macro-namespaces frontend))
        macro-ns      (or macro-ns #{})
        dep-syms      (->> ns-nodes
                           (mapcat namespace/ns-dependency-syms)
                           (remove macro-ns))
        dep-syms      (cond-> dep-syms
                              (not= self-ns 'cljh.core)
                              (conj 'cljh.core))]
    (ip/register-deps compile-ctx dep-syms)
    (let [user-aliases (reduce merge {}
                               (map (fn [ns-node]
                                      (namespace/ns-reference-aliases
                                        ns-node
                                        (ip/symbol-table compile-ctx)))
                                    ns-nodes))
          std-aliases  (namespace/ns-exported-syms
                         (ip/symbol-table compile-ctx) 'cljh.core)
          aliases      (merge user-aliases std-aliases)
          env0         {:ns-name     self-ns
                        :aliases     aliases
                        :bound-vars  #{}
                        :global-defs #{}}
          [qualified-non-ns _]
          (reduce (fn [[nodes env] root]
                    (let [[new-root new-env] (resolve-node* root env)]
                      [(conj nodes new-root) new-env]))
                  [[] env0]
                  non-ns-roots)]
      (into (vec ns-nodes) qualified-non-ns))))