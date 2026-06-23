(ns top.kzre.homunculus.core.types.module.resolve-ns
  "命名空间解析：提取 ns 声明，注册依赖，替换别名，并正规化本模块符号。
   使用多方法分派遍历 AST，维护词法作用域与全局定义环境。
   Clojure 语义：所有 define 定义的符号均添加命名空间前缀。"
  (:require
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as types]
    [top.kzre.homunculus.core.types.namespace :as ns-info]
    [top.kzre.homunculus.internal.protocol :as ip]))

;; ── 环境操作 ──
(defn- add-locals [env var-names]
  (update env :locals into var-names))

(defn- add-global-def [env def-name]
  (update env :global-defs conj def-name))

;; ── 多方法分派：返回 [new-node, new-env] ──
(defmulti resolve-node
          (fn [node env] (n/kind node)))

(defmethod resolve-node :ns [node env]
  [node env])

;; ★ define 节点：始终正规化名称，并更新全局定义集合
(defmethod resolve-node :define [node env]
  (let [old-name (n/define-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        unqualified-name (symbol (name old-name))
        env'     (add-global-def env unqualified-name)
        val-node (n/define-val node)
        [new-val env''] (if val-node (resolve-node val-node env') [nil env'])]
    [(n/make-define new-name new-val
                    (n/define-doc node)
                    (n/attrs node)
                    (n/node-meta node)
                    (n/parent node))
     env'']))

;; lambda：进入新作用域，添加参数到局部变量，环境不变传出
(defmethod resolve-node :lambda [node env]
  (let [params (n/lambda-params node)
        param-names (map n/var-name params)
        env' (-> env
                 (assoc :toplevel? false)
                 (add-locals param-names))
        [new-body _] (resolve-node (n/lambda-body node) env')]
    [(n/make-lambda params new-body
                    (n/lambda-captures node)
                    (n/lambda-fn-name node)
                    (n/attrs node)
                    (n/node-meta node)
                    (n/parent node))
     env]))

;; let：新作用域，添加局部变量，环境不变传出
(defmethod resolve-node :let [node env]
  (let [bindings (n/let-bindings node)
        binding-names (set (map (comp n/var-name first) bindings))
        env-inner (-> env
                      (assoc :toplevel? false)
                      (add-locals binding-names))
        [new-vals env'] (reduce (fn [[vs e] [v expr]]
                                  (let [[new-expr e2] (resolve-node expr e)]
                                    [(conj vs [v new-expr]) e2]))
                                [[] env]
                                bindings)
        [new-body _] (resolve-node (n/let-body node) env-inner)]
    [(n/make-let new-vals new-body
                 (n/attrs node)
                 (n/node-meta node)
                 (n/parent node))
     env]))

;; loop：类似 let
(defmethod resolve-node :loop [node env]
  (let [bindings (n/loop-bindings node)
        binding-names (set (map (comp n/var-name first) bindings))
        env-inner (-> env
                      (assoc :toplevel? false)
                      (add-locals binding-names))
        [new-bindings env'] (reduce (fn [[bnds e] [v expr]]
                                      (let [[new-expr e2] (resolve-node expr e)]
                                        [(conj bnds [v new-expr]) e2]))
                                    [[] env]
                                    bindings)
        [new-body _] (resolve-node (n/loop-body node) env-inner)]
    [(n/make-loop new-bindings new-body
                  (n/attrs node)
                  (n/node-meta node)
                  (n/parent node))
     env]))

;; block：顺序处理，传递环境
(defmethod resolve-node :block [node env]
  (let [exprs (n/block-exprs node)
        [new-exprs env'] (reduce (fn [[es e] expr]
                                   (let [[new-expr e2] (resolve-node expr e)]
                                     [(conj es new-expr) e2]))
                                 [[] env]
                                 exprs)]
    [(n/make-block new-exprs
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))
     env']))

;; call：递归子节点，传递环境
(defmethod resolve-node :call [node env]
  (let [[new-fn env1] (resolve-node (n/call-fn node) env)
        [new-args env2] (reduce (fn [[args e] arg]
                                  (let [[new-arg e2] (resolve-node arg e)]
                                    [(conj args new-arg) e2]))
                                [[] env1]
                                (n/call-args node))]
    [(n/make-call new-fn new-args
                  (n/attrs node)
                  (n/node-meta node)
                  (n/parent node))
     env2]))

;; if：顺序处理，传递环境
(defmethod resolve-node :if [node env]
  (let [[new-test env1] (resolve-node (n/if-test node) env)
        [new-then env2] (resolve-node (n/if-then node) env1)
        [new-else env3] (if-let [e (n/if-else node)]
                          (resolve-node e env2)
                          [nil env2])]
    [(n/make-if new-test new-then new-else
                (n/attrs node)
                (n/node-meta node)
                (n/parent node))
     env3]))

;; while：顺序处理
(defmethod resolve-node :while [node env]
  (let [[new-test env1] (resolve-node (n/while-test node) env)
        [new-body env2] (resolve-node (n/while-body node) env1)]
    [(n/make-while new-test new-body
                   (n/attrs node)
                   (n/node-meta node)
                   (n/parent node))
     env2]))

;; assign：顺序处理
(defmethod resolve-node :assign [node env]
  (let [[new-var env1] (resolve-node (n/assign-var node) env)
        [new-val env2] (resolve-node (n/assign-val node) env1)]
    [(n/make-assign new-var new-val
                    (n/attrs node)
                    (n/node-meta node)
                    (n/parent node))
     env2]))

;; ★ 变量引用：根据环境决定是否正规化
(defmethod resolve-node :variable [node env]
  (let [var-name (n/var-name node)
        new-node (cond
                   ;; 1. 已有命名空间，可能是别名引用，替换为全限定名
                   (namespace var-name)
                   (if-let [full-ns (get (:aliases env) (symbol (namespace var-name)))]
                     (n/variable-with-name node (symbol (str full-ns) (name var-name)))
                     node)

                   ;; 2. 局部变量（函数参数、let 绑定等）保持原样
                   (contains? (:locals env) var-name)
                   node

                   ;; 3. 别名引入的无命名空间符号（如通过 :refer :all 或标准库别名）
                   (contains? (:aliases env) var-name)
                   (n/variable-with-name node (get (:aliases env) var-name))

                   ;; 4. 本模块顶层定义的符号，加上当前命名空间前缀
                   (contains? (:global-defs env) var-name)
                   (n/variable-with-name node (symbol (str (:self-ns env)) (name var-name)))

                   ;; 5. 其他未知符号保持原样
                   :else
                   node)]
    [new-node env]))

;; ── 新增：数组和其他特殊节点的递归处理 ──

(defmethod resolve-node :new-array [node env]
  (let [[new-size env'] (resolve-node (n/new-array-size node) env)]
    [(n/make-new-array new-size (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :aget [node env]
  (let [[new-target env1] (resolve-node (n/aget-target node) env)
        [new-idx env2] (resolve-node (n/aget-idx node) env1)]
    [(n/make-aget new-target new-idx (n/node-meta node) (n/parent node))
     env2]))

(defmethod resolve-node :aset [node env]
  (let [[new-target env1] (resolve-node (n/aset-target node) env)
        [new-idx env2] (resolve-node (n/aset-idx node) env1)
        [new-val env3] (resolve-node (n/aset-val node) env2)]
    [(n/make-aset new-target new-idx new-val (n/node-meta node) (n/parent node))
     env3]))

(defmethod resolve-node :alength [node env]
  (let [[new-target env'] (resolve-node (n/alength-target node) env)]
    [(n/make-alength new-target (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :vector [node env]
  (let [items (n/vector-items node)
        [new-items env'] (reduce (fn [[is e] item]
                                   (let [[new-item e2] (resolve-node item e)]
                                     [(conj is new-item) e2]))
                                 [[] env]
                                 items)]
    [(n/make-vector new-items (n/attrs node) (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :map [node env]
  (let [kvs (n/map-kvs node)
        pairs (partition 2 kvs)
        [new-kvs env'] (reduce (fn [[new-kv-pairs e] [k v]]
                                 (let [[new-k e1] (resolve-node k e)
                                       [new-v e2] (resolve-node v e1)]
                                   [(conj new-kv-pairs new-k new-v) e2]))
                               [[] env]
                               pairs)]
    [(n/make-map (vec new-kvs) (n/attrs node) (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :member-access [node env]
  (let [[new-target env1] (resolve-node (n/access-target node) env)
        args (n/access-args node)
        [new-args env2] (reduce (fn [[as e] arg]
                                  (let [[new-arg e2] (resolve-node arg e)]
                                    [(conj as new-arg) e2]))
                                [[] env1]
                                args)]
    [(n/make-member-access new-target (n/access-member node) new-args
                           (n/node-meta node) (n/parent node))
     env2]))

(defmethod resolve-node :convert [node env]
  (let [[new-expr env'] (resolve-node (n/convert-expr node) env)]
    [(n/make-convert new-expr (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                     (n/attrs node) (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :try [node env]
  (let [[new-body env1] (resolve-node (n/try-body node) env)
        [new-catches env2] (reduce (fn [[cs e] c]
                                     (let [[new-c e2] (resolve-node c e)]
                                       [(conj cs new-c) e2]))
                                   [[] env1]
                                   (n/try-catches node))
        [new-finally env3] (if-let [f (n/try-finally node)]
                             (resolve-node f env2)
                             [nil env2])]
    [(n/make-try new-body new-catches new-finally
                 (n/attrs node) (n/node-meta node) (n/parent node))
     env3]))

(defmethod resolve-node :catch [node env]
  (let [[new-class env1] (resolve-node (n/catch-class node) env)
        [new-sym env2] (resolve-node (n/catch-sym node) env1)
        [new-body env3] (reduce (fn [[bs e] expr]
                                  (let [[new-expr e2] (resolve-node expr e)]
                                    [(conj bs new-expr) e2]))
                                [[] env2]
                                (n/catch-body node))]
    [(n/make-catch new-class new-sym new-body
                   (n/attrs node) (n/node-meta node) (n/parent node))
     env3]))

(defmethod resolve-node :throw [node env]
  (let [[new-expr env'] (resolve-node (n/throw-expr node) env)]
    [(n/make-throw new-expr (n/attrs node) (n/node-meta node) (n/parent node))
     env']))

(defmethod resolve-node :recur [node env]
  (let [args (n/recur-args node)
        [new-args env'] (reduce (fn [[as e] arg]
                                  (let [[new-arg e2] (resolve-node arg e)]
                                    [(conj as new-arg) e2]))
                                [[] env]
                                args)]
    [(n/make-recur new-args (n/attrs node) (n/node-meta node) (n/parent node))
     env']))

;; record：名称正规化
(defmethod resolve-node :record [node env]
  (let [old-name (n/record-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        fields (n/record-fields node)
        [new-fields env'] (reduce (fn [[fs e] f]
                                    (if-let [init (n/field-init f)]
                                      (let [[new-init e2] (resolve-node init e)]
                                        [(conj fs (n/field-with-init f new-init)) e2])
                                      [(conj fs f) e]))
                                  [[] env]
                                  fields)
        protocols (n/record-protocols node)]
    [(n/make-record new-name new-fields protocols
                    (n/attrs node)
                    (n/node-meta node)
                    (n/parent node))
     env]))

;; protocol：名称正规化
(defmethod resolve-node :protocol [node env]
  (let [old-name (n/protocol-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        funcs (n/protocol-funcs node)]
    [(n/make-protocol new-name funcs
                      (n/attrs node)
                      (n/node-meta node)
                      (n/parent node))
     env]))

;; 默认：字面量等直接返回原节点，环境不变
(defmethod resolve-node :default [node env]
  [node env])

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
        ;; 自动追加标准库依赖
        dep-syms (cond-> dep-syms
                         (not= self-ns 'cljh.core)
                         (conj 'cljh.core))]
    ;; ★ 先注册依赖（触发编译，填充符号表）
    (ip/register-deps context dep-syms)
    ;; ★ 现在符号表已包含依赖模块的符号，再构建别名映射
    (let [user-aliases (reduce merge {}
                          (map (fn [ns-node]
                                 (ns-info/ns-reference-aliases ns-node (ip/symbol-table context)))
                               ns-nodes))
          ;; 显式添加标准库 cljh.core 的所有导出符号作为别名
          std-aliases (ns-info/ns-exported-syms (ip/symbol-table context) 'cljh.core)
          aliases (merge user-aliases std-aliases)
          env0 {:self-ns self-ns :aliases aliases :locals #{} :global-defs #{} :toplevel? true}
          [qualified-non-ns _]
          (reduce (fn [[nodes env] root]
                    (let [[new-root new-env] (resolve-node root env)]
                      [(conj nodes new-root) new-env]))
                  [[] env0]
                  non-ns-roots)]
      (into (vec ns-nodes) qualified-non-ns))))