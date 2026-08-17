(ns top.kzre.homunculus.core.irstmt.assign-propagate
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defrecord Env [substs])
(defn make-env [] (->Env {}))
;; ── 添加映射 ──
(defn- env-add-subst [env a b]
  (assoc-in env [:substs a] b))

(defn- env-get-subst [env a]
  (get-in env [:substs a]))

(defmulti propagate-node (fn [node _env] (ast/kind node)))

(defn walk [node env]
  (ast/reduce-children node propagate-node env))

;; 默认：递归所有子节点
(defmethod propagate-node :default [node env]
  (walk node env))


;; ── 变量引用：替换为映射目标 ──
(defmethod propagate-node :variable
  [node env]
  (let [var-name (:name node)]
    (if-let [subst (env-get-subst env var-name)]
      [(assoc node :name subst) env]
      [node env])))

;; ── 赋值：右值若是变量则建立映射，删除赋值 ──
(defmethod propagate-node :assign
  [node env]
  (let [[val-node env1] (propagate-node (:val node) env)
        [var-node env2] (propagate-node (:var node) env1)]
    (if (and val-node (= :variable (ast/kind val-node)))
      [nil
       (env-add-subst env1 (:name var-node) (:name val-node))]
      [(assoc node :var var-node :val val-node) env2])))

;; ── 变量声明：初始化若是变量则建立映射，保留声明 ──
(defmethod propagate-node :var-decl
  [node env]
  (if-let [init (:val node)]
    (let [[init' env'] (propagate-node init env)]
      (if (= :variable (ast/kind init'))
        [(assoc node :val init')
         (env-add-subst env' (:name node) (:name init'))]
        [(assoc node :val init') env']))
    [node env]))

;; ── 函数定义：内部新环境，映射隔离 ──
(defmethod propagate-node :function
  [node env]
  (let [inner-env (make-env)
        [new-body _] (propagate-node (:body node) inner-env)]
    [(assoc node :body new-body) env]))

(defn flatten-body [body]
  (let [stmts (:stmts body)
        ret   (:ret body)]
    (if ret
      (assoc body :stmts (conj stmts ret) :ret nil)
      body)))

(defmethod propagate-node :while
  [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        [body env2] (propagate-node (:body node) env1)
        new-body (flatten-body body)]
    [(assoc node :test test' :body new-body) env2]))

(defmethod propagate-node :if
  [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        [then' env2] (propagate-node (:then node) env1)
        [else' env3] (if-let [e (:else node)]
                       (propagate-node e env2)
                       [nil env2])]
    [(assoc node :test test'
                 :then (flatten-body then')
                 :else (flatten-body else'))
     env3]))

;; ── 顶层入口：处理所有顶层节点 ──
(defn propagate-nodes [nodes]
  (let [env (make-env)]
    (mapv #(first (propagate-node % env)) nodes)))