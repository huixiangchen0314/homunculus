(ns top.kzre.homunculus.core.types.module.resolve-ns
  "命名空间解析。利用 reduce-children 自动遍历，只保留需要环境/名称特殊处理的节点。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as types]
    [top.kzre.homunculus.core.types.namespace :as namespace]
    [top.kzre.homunculus.internal.protocol :as ip]))

;; TODO 废弃，动态计算，而不是预处理展开
;; ── 环境辅助函数 ──
(defn- add-locals [env var-names]
  (update env :locals into var-names))

(defn- add-global-def [env def-name]
  (update env :global-defs conj def-name))

;; ── 多方法分派 ──
(defmulti resolve-node
          (fn [node env] (n/kind node)))

;; ★ ns：不做变换
(defmethod resolve-node :ns [node env]
  [node env])

;; ★ define：正规化名称 + 注册全局定义，然后递归值表达式
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
                    (n/define-docstring node)
                    (n/attrs node)
                    (n/node-meta node))
     env'']))

;; ★ lambda：创建新作用域，参数加入局部变量，不替换参数
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
                    (n/node-meta node))
     env]))

;; ★ let：创建新作用域，绑定变量加入局部变量，顺序处理值表达式和体
(defmethod resolve-node :let [node env]
  (let [bindings (n/let-bindings node)          ;; 现在返回 Binding 向量
        binding-names (set (map #(:name (:var %)) bindings))
        env-inner (-> env
                      (assoc :toplevel? false)
                      (add-locals binding-names))
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
(defmethod resolve-node :loop [node env]
  (let [bindings (n/loop-bindings node)                    ;; 现在返回 Binding 向量
        binding-names (set (map #(:name (:var %)) bindings))
        env-inner (-> env
                      (assoc :toplevel? false)
                      (add-locals binding-names))
        [new-bindings _env'] (reduce (fn [[bnds e] b]       ;; b 是 Binding 记录
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
(defmethod resolve-node :variable [node env]
  (let [var-name (n/var-name node)
        new-node (cond
                   (namespace var-name)
                   (if-let [full-ns (get (:aliases env) (symbol (namespace var-name)))]
                     (n/variable-with-name node (symbol (str full-ns) (name var-name)))
                     node)
                   (contains? (:locals env) var-name) node
                   (contains? (:aliases env) var-name)
                   (n/variable-with-name node (get (:aliases env) var-name))
                   (contains? (:global-defs env) var-name)
                   (n/variable-with-name node (symbol (str (:self-ns env)) (name var-name)))
                   :else node)]
    [new-node env]))

;; ★ catch：异常符号是绑定变量，不参与替换；只递归 body
(defmethod resolve-node :catch [node env]
  (let [body (n/catch-body node)
        [new-body env'] (reduce (fn [[bs e] expr]
                                  (let [[new-expr e2] (resolve-node expr e)]
                                    [(conj bs new-expr) e2]))
                                [[] env]
                                body)]
    [(n/make-catch (n/catch-class node) (n/catch-sym node) new-body
                   (n/attrs node) (n/node-meta node))
     env']))

;; ★ record / protocol：名称正规化，其余子节点由 reduce-children 自动处理（见默认方法）
(defmethod resolve-node :record [node env]
  (let [old-name (n/record-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        ;; 先正规化名称，然后委托给默认方法处理子节点（包含字段初始化、方法体等）
        ;; 这里我们手动构造新名称节点，再用 reduce-children 遍历子节点
        [new-node env'] ((get-method resolve-node :default)
                         (n/record-with-name node new-name) env)]
    [new-node env']))

(defmethod resolve-node :protocol [node env]
  (let [old-name (n/protocol-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (:self-ns env)) (name old-name)))
        [new-node env'] ((get-method resolve-node :default)
                         (n/protocol-with-name node new-name) env)]
    [new-node env']))

;; ★ 默认方法：利用 reduce-children 自动遍历并重建节点
;;   将环境传递下去，最终返回 [重建后的节点, 最终环境]
(defmethod resolve-node :default [node env]
  (ir2/reduce-children node
                        (fn [child current-env]
                          (resolve-node child current-env))
                        env))

;; ── 主函数不变 ──
(defn resolve-ns
  [ir2-roots context frontend]
  (let [ns-nodes      (filter #(= (ir2/kind %) :ns) ir2-roots)
        non-ns-roots  (remove #(= (ir2/kind %) :ns) ir2-roots)
        self-ns       (some-> ns-nodes first :name)
        _             (when (nil? self-ns)
                        (throw (ex-info "No namespace declaration found" {})))
        macro-ns      (when frontend (types/macro-namespaces frontend))
        macro-ns      (or macro-ns #{})
        dep-syms (->> ns-nodes
                      (mapcat namespace/ns-dependency-syms)
                      (remove macro-ns))
        dep-syms (cond-> dep-syms
                         (not= self-ns 'cljh.core)
                         (conj 'cljh.core))]
    (ip/register-deps context dep-syms)
    (let [user-aliases (reduce merge {}
                               (map (fn [ns-node]
                                      (namespace/ns-reference-aliases ns-node (ip/symbol-table context)))
                                    ns-nodes))
          std-aliases (namespace/ns-exported-syms (ip/symbol-table context) 'cljh.core)
          aliases (merge user-aliases std-aliases)
          env0 {:self-ns self-ns :aliases aliases :locals #{} :global-defs #{} :toplevel? true}
          [qualified-non-ns _]
          (reduce (fn [[nodes env] root]
                    (let [[new-root new-env] (resolve-node root env)]
                      [(conj nodes new-root) new-env]))
                  [[] env0]
                  non-ns-roots)]
      (into (vec ns-nodes) qualified-non-ns))))