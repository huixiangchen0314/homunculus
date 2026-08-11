(ns top.kzre.homunculus.backend.shader.assign-propagate
  "ShaderAST 变量替换：将变量引用替换为另一个变量，不消除定义。
   作用域：函数/入口点内部独立环境，外部不可见。"
  (:require [top.kzre.homunculus.backend.shader.ast :as ast]))

;; 变量维持一个定义的记录，如果变量可能为脏
(defrecord DefinitionScope [var-name def dirty?])

;; ── 环境：变量 -> 表达式 (目前仅支持变量到变量) ──
(defrecord Env [substs])
(defn make-env [] (->Env {}))
;; ── 添加映射 ──
(defn- env-add-subst [env a b]
  (assoc-in env [:substs a] b))

(defn- env-get-subst [env a]
  (get-in env [:substs a]))


(defmulti propagate-node (fn [node _env] (ast/kind node)))

;; 默认：递归所有子节点
(defmethod propagate-node :default [node env]
  (ast/reduce-children node propagate-node env))

;; ── 变量引用：替换为映射目标 ──
(defmethod propagate-node :variable
  [node env]
  (let [var-name (:name node)]
    (if-let [subst (env-get-subst env var-name)]
      (let [new-var (ast/->Variable subst (:meta node))]
        [new-var env])
      [node env])))

;; ── 赋值：右值若是变量则建立映射，保留赋值 ──
(defmethod propagate-node :assign
  [node env]
  (let [rhs (:rhs node)
        [rhs' env1] (propagate-node rhs env)
        lhs (:lhs node)
        [lhs' env2] (propagate-node lhs env1)]
    (if (and rhs' (= :variable (ast/kind rhs')))
      [nil
       (env-add-subst env1 (:name (:lhs node)) (:name rhs'))]
      [(assoc node :lhs lhs' :rhs rhs') env2])))

;; ── 变量声明：初始化若是变量则建立映射，保留声明 ──
(defmethod propagate-node :var-decl
  [node env]
  (if-let [init (:init node)]
    (let [[init' env'] (propagate-node init env)]
      (if (= :variable (ast/kind init'))
        [(assoc node :init init')
         (env-add-subst env' (:name node) (:name init'))]
        [(assoc node :init init') env']))
    [node env]))

;; ── 函数定义：内部新环境，映射隔离 ──
(defmethod propagate-node :function
  [node env]
  (let [inner-env (make-env)
        [new-body _] (propagate-node (:body node) inner-env)]
    [(assoc node :body new-body) env]))

;; ── 入口点：内部新环境，映射隔离 ──
(defmethod propagate-node :entry-point
  [node env]
  (let [inner-env (make-env)
        [new-body _] (propagate-node (:body node) inner-env)]
    [(assoc node :body new-body) env]))

;; ── 顶层入口：处理所有顶层节点 ──
(defn propagate-nodes [nodes]
  (let [env (make-env)]
    (mapv #(first (propagate-node % env)) nodes)))