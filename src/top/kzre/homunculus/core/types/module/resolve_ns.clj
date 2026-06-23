(ns top.kzre.homunculus.core.types.module.resolve-ns
  "命名空间解析：提取 ns 声明，注册依赖，替换别名，并正规化本模块符号。
   使用多方法分派遍历 AST，维护词法作用域，仅对正确的符号进行重命名。"
  (:require
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as types]
    [top.kzre.homunculus.core.types.namespace :as ns-info]
    [top.kzre.homunculus.internal.protocol :as ip]))

;; ── 环境辅助 ──
(defn- add-locals [env var-names]
  (update env :locals into var-names))

;; ── 多方法分派：node 的类型 ──
(defmulti resolve-node (fn [node _env] (n/kind node)))

(defmethod resolve-node :ns [node env] node)

(defmethod resolve-node :define [node env]
  (let [old-name (n/define-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        val-node (n/define-val node)
        new-val  (when val-node (resolve-node val-node env))]
    (n/make-define new-name new-val
                   (n/define-doc node)
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))))

(defmethod resolve-node :lambda [node env]
  (let [params (n/lambda-params node)
        param-names (map n/var-name params)
        env' (add-locals env param-names)
        new-body (resolve-node (n/lambda-body node) env')]
    (n/make-lambda params new-body
                   (n/lambda-captures node)
                   (n/lambda-fn-name node)
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))))

(defmethod resolve-node :let [node env]
  (let [bindings (n/let-bindings node)
        binding-names (set (map (comp n/var-name first) bindings))
        env' (add-locals env binding-names)
        new-bindings (mapv (fn [[v e]]
                             [(resolve-node v env) (resolve-node e env)])
                           bindings)
        new-body (resolve-node (n/let-body node) env')]
    (n/make-let new-bindings new-body
                (n/attrs node)
                (n/node-meta node)
                (n/parent node))))

(defmethod resolve-node :loop [node env]
  (let [bindings (n/loop-bindings node)
        binding-names (set (map (comp n/var-name first) bindings))
        env' (add-locals env binding-names)
        new-bindings (mapv (fn [[v e]]
                             [(resolve-node v env) (resolve-node e env)])
                           bindings)
        new-body (resolve-node (n/loop-body node) env')]
    (n/make-loop new-bindings new-body
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node))))

(defmethod resolve-node :block [node env]
  (let [exprs (n/block-exprs node)
        new-exprs (mapv #(resolve-node % env) exprs)]
    (n/make-block new-exprs
                  (n/attrs node)
                  (n/node-meta node)
                  (n/parent node))))

(defmethod resolve-node :call [node env]
  (let [fn-node (n/call-fn node)
        new-fn (resolve-node fn-node env)
        args (n/call-args node)
        new-args (mapv #(resolve-node % env) args)]
    (n/make-call new-fn new-args
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node))))

(defmethod resolve-node :if [node env]
  (let [new-test (resolve-node (n/if-test node) env)
        new-then (resolve-node (n/if-then node) env)
        else-node (n/if-else node)
        new-else (when else-node (resolve-node else-node env))]
    (n/make-if new-test new-then new-else
               (n/attrs node)
               (n/node-meta node)
               (n/parent node))))

(defmethod resolve-node :while [node env]
  (let [new-test (resolve-node (n/while-test node) env)
        new-body (resolve-node (n/while-body node) env)]
    (n/make-while new-test new-body
                  (n/attrs node)
                  (n/node-meta node)
                  (n/parent node))))

(defmethod resolve-node :assign [node env]
  (let [new-var (resolve-node (n/assign-var node) env)
        new-val (resolve-node (n/assign-val node) env)]
    (n/make-assign new-var new-val
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))))

(defmethod resolve-node :variable [node env]
  (let [var-name (n/var-name node)]
    (if (namespace var-name)
      (if-let [full-ns (get (:aliases env) (symbol (namespace var-name)))]
        (n/variable-with-name node (symbol (str full-ns) (name var-name)))
        node)
      (if (contains? (:locals env) var-name)
        node
        (n/variable-with-name node (symbol (str (:self-ns env)) (name var-name)))))))

(defmethod resolve-node :record [node env]
  (let [old-name (n/record-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        fields (n/record-fields node)
        new-fields (mapv (fn [f]
                           (if-let [init (n/field-init f)]
                             (n/field-with-init f (resolve-node init env))
                             f))
                         fields)
        protocols (n/record-protocols node)]
    (n/make-record new-name new-fields protocols
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))))

(defmethod resolve-node :protocol [node env]
  (let [old-name (n/protocol-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        funcs (n/protocol-funcs node)]
    (n/make-protocol new-name funcs
                     (n/attrs node)
                     (n/node-meta node)
                     (n/parent node))))

;; 默认：字面量、vector、map、convert、try/catch/throw、recur、new-array 等直接返回原节点
(defmethod resolve-node :default [node _] node)

;; ── 主函数 ──
(defn resolve-ns
  [ir2-roots context frontend]
  (let [ns-nodes      (filter #(= (ir2p/kind %) :ns) ir2-roots)
        non-ns-roots  (remove #(= (ir2p/kind %) :ns) ir2-roots)
        self-ns       (some-> ns-nodes first :name)
        _             (when (nil? self-ns)
                        (throw (ex-info "No namespace declaration found" {})))
        macro-ns      (when frontend (types/macro-namespaces frontend))
        macro-ns      (or macro-ns #{})
        dep-syms (->> ns-nodes
                      (mapcat ns-info/ns-dependency-syms)
                      (remove macro-ns))
        aliases (reduce merge {} (map ns-info/ns-reference-aliases ns-nodes))]
    (ip/register-deps context dep-syms)
    (let [env {:self-ns self-ns :aliases aliases :locals #{}}
          qualified-non-ns (mapv #(resolve-node % env) non-ns-roots)]
      (into (vec ns-nodes) qualified-non-ns))))