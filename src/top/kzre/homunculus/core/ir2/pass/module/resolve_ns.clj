(ns top.kzre.homunculus.core.ir2.pass.module.resolve-ns
  "命名空间解析。利用 reduce-children 自动遍历，只保留需要环境/名称特殊处理的节点。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.module.env :as env]))

;; ── 多方法分派 ──
(defmulti resolve-node*
          (fn [node env] (n/kind node)))

(defn- resolve-map-symbols
  [x env]
  (walk/postwalk
    (fn [v] (if (symbol? v) (env/resolve-symbol env v) v))
    x))

(defn- resolve-meta
  [node env]
  (if-let [md (n/node-meta node)]
    (assoc node :meta (resolve-map-symbols md env))
    node))

(defn resolve-node
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

;; ★ define
(defmethod resolve-node* :define [node env]
  (let [old-name (n/define-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (env/module-ns env)) (name old-name)))
        env'     (env/add-global-def env (symbol (name old-name)))
        val-node (n/define-val node)
        [new-val env''] (if val-node (resolve-node val-node env') [nil env'])]
    [(n/make-define new-name new-val
                    (n/define-docstring node)
                    (n/attrs node)
                    (n/node-meta node))
     env'']))

;; ★ lambda
(defmethod resolve-node* :lambda [node env]
  (let [params      (n/lambda-params node)
        env'        (env/bind-vars env (map n/var-name params))
        [new-body _] (resolve-node (n/lambda-body node) env')]
    [(n/make-lambda params new-body
                    (n/lambda-captures node)
                    (n/lambda-fn-name node)
                    (n/attrs node)
                    (n/node-meta node))
     env]))

;; ★ let
(defmethod resolve-node* :let [node env]
  (let [bindings      (n/let-bindings node)
        env-inner     (env/bind-vars env (set (map #(:name (:var %)) bindings)))
        [new-bindings env']
        (reduce (fn [[bnds e] b]
                  (let [var-node (:var b)
                        [new-val e2] (resolve-node (:val b) e)]
                    [(conj bnds (assoc b :var var-node :val new-val)) e2]))
                [[] env]
                bindings)
        [new-body _] (resolve-node (n/let-body node) env-inner)]
    [(ir2/->Let new-bindings new-body (:attrs node) (:meta node))
     env]))

;; ★ loop
(defmethod resolve-node* :loop [node env]
  (let [bindings      (n/loop-bindings node)
        env-inner     (env/bind-vars env (set (map #(:name (:var %)) bindings)))
        [new-bindings _env']
        (reduce (fn [[bnds e] b]
                  (let [[new-expr e2] (resolve-node (:val b) e)]
                    [(conj bnds (assoc b :val new-expr)) e2]))
                [[] env]
                bindings)
        [new-body _] (resolve-node (n/loop-body node) env-inner)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node))
     env]))

;; ★ variable
(defmethod resolve-node* :variable [node env]
  (let [var-name (n/var-name node)
        new-node (n/variable-with-name node (env/resolve-symbol env var-name))]
    [new-node env]))

;; ★ catch
(defmethod resolve-node* :catch [node env]
  (let [sym       (n/catch-sym node)
        sym-name  (n/var-name sym)
        inner-env (env/add-local-def env sym-name)
        [new-body _] (reduce (fn [[bs _] expr]
                               (let [[new-expr _] (resolve-node expr inner-env)]
                                 [(conj bs new-expr) inner-env]))
                             [[] inner-env]
                             (n/catch-body node))]
    [(n/make-catch (n/catch-class node) sym new-body
                   (n/attrs node) (n/node-meta node))
     env]))

;; ★ record
(defmethod resolve-node* :record [node env]
  (let [old-name (n/record-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (env/module-ns env)) (name old-name)))
        base     (name old-name)
        env'     (-> env
                     (env/add-global-def (symbol base))
                     (env/add-global-def (symbol (str "->" base)))
                     (env/add-global-def (symbol (str "map->" base))))
        [new-node env''] (walk (n/record-with-name node new-name) env')]
    [new-node env'']))

;; ★ protocol
(defmethod resolve-node* :protocol [node env]
  (let [old-name (n/protocol-name node)
        new-name (if (namespace old-name)
                   old-name
                   (symbol (str (env/module-ns env)) (name old-name)))
        env'     (env/add-global-def env (symbol (name old-name)))
        [new-node env''] (walk (n/protocol-with-name node new-name) env')]
    [new-node env'']))

;; ★ 默认
(defmethod resolve-node* :default [node env]
  (walk node env))

;; ── 主函数 ──
(defn resolve-ns
  [ir2-roots init-env]
  (let [ns-nodes     (filter #(= :ns (ir2/kind %)) ir2-roots)
        non-ns-roots (remove #(= :ns (ir2/kind %)) ir2-roots)
        [qualified-non-ns _]
        (reduce (fn [[nodes e] root]
                  (let [[new-root new-e] (resolve-node* root e)]
                    [(conj nodes new-root) new-e]))
                [[] init-env]
                non-ns-roots)]
    (into (vec ns-nodes) qualified-non-ns)))