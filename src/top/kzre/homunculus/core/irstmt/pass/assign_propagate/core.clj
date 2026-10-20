(ns top.kzre.homunculus.core.irstmt.pass.assign-propagate.core
  "SSA 赋值传播：消除 `x = y`（x 是 SSA 且非可变类型），用 y 替换后续 use(x)。

   消除条件（必须全部满足）：
   1. x 是 SSA（def-count == 1）—— 保证全函数内 x 只定义一次，
      消除不会让某路径上的 x 未定义
   2. x 的类型非可变（mutable-variable? == false）—— 可变类型的 x = y
      消除后，若 y 后续被修改，x 的 use 会看到被修改后的值，语义错误

   分工：assign-propagate 负责替换，DCE 负责删除不再被引用的声明。

   作用域：
   - block / if 分支 / while body 各自隔离 substs
   - function / method 进入新作用域时重新 scan"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.assign-propagate.env :as env]))

(defmulti propagate-node (fn [node _env] (ast/kind node)))

;; ── 默认：递归所有子节点 ──
(defmethod propagate-node :default [node env]
  (if (nil? node)
    [node env]
    (ast/rreduce-children node propagate-node env)))

;; ── 变量引用：替换 ──
(defmethod propagate-node :variable [node env]
  (if-let [target (env/subst env (:name node))]
    [(assoc node :name target) env]
    [node env]))

;; ── assign ──
(defmethod propagate-node :assign [node env]
  (let [var (:var node)
        val (:val node)]
    (cond
      ;; 自赋值：删除
      (and (= :variable (ast/kind var))
           (= :variable (ast/kind val))
           (= (:name var) (:name val)))
      [nil env]

      ;; SSA 且非可变：消除，建立映射
      (and (= :variable (ast/kind var))
           (= :variable (ast/kind val))
           (env/ssa? env (:name var))
           (not (env/mutable-value? env var)))
      [nil (env/add-subst
             (env/remove-subst env (:name var))
             (:name var)
             (:name val))]

      ;; 其他：保留，清除 var 的旧映射
      :else
      (let [[var' env1] (propagate-node var env)
            [val' env2] (propagate-node val env1)
            env3 (if (= :variable (ast/kind var'))
                   (env/remove-subst env2 (:name var'))
                   env2)]
        [(assoc node :var var' :val val') env3]))))

;; ── var-decl ──
(defmethod propagate-node :var-decl [node env]
  (let [name (:name node)
        init (:val node)]
    (cond
      (nil? init)
      [node (env/remove-subst env name)]

      ;; SSA 且非可变：建立映射，声明本身保留（DCE 决定是否删除）
      (and (= :variable (ast/kind init))
           (env/ssa? env name)
           (not (env/mutable-value? env init)))
      [(assoc node :val init)
       (env/add-subst (env/remove-subst env name) name (:name init))]

      :else
      (let [[init' env'] (propagate-node init env)]
        [(assoc node :val init')
         (env/remove-subst env' name)]))))

;; ── block：局部 substs ──
(defmethod propagate-node :block [node env]
  (let [outer (env/substs-of env)
        [stmts' _]
        (reduce (fn [[acc e] stmt]
                  (let [[s' e'] (propagate-node stmt e)]
                    [(if s' (conj acc s') acc) e']))
                [[] env]
                (:stmts node))
        [ret' _] (if-let [r (:ret node)]
                   (propagate-node r env)
                   [nil env])]
    [(ast/->Block stmts' ret'
                  (ast/attrs node) (ast/node-meta node))
     (env/with-substs env outer)]))

;; ── if：两分支独立 ──
(defmethod propagate-node :if [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        base (env/substs-of env1)
        [then' _] (propagate-node (:then node)
                                  (env/with-substs env1 base))
        [else' _] (if-let [e (:else node)]
                    (propagate-node e (env/with-substs env1 base))
                    [nil env1])]
    [(assoc node :test test' :then then' :else else')
     (env/with-substs env1 base)]))

;; ── while：body substs 不泄漏 ──
(defmethod propagate-node :while [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        base (env/substs-of env1)
        [body' _] (propagate-node (:body node)
                                  (env/with-substs env1 base))]
    [(assoc node :test test' :body body')
     (env/with-substs env1 base)]))

;; ── function / method：新作用域，重新 scan ──
(defmethod propagate-node :function [node env]
  (let [inner-env (env/with-function-scope env node)
        [body' _] (propagate-node (:body node) inner-env)]
    [(assoc node :body body') env]))

(defmethod propagate-node :method [node env]
  (let [inner-env (env/with-function-scope env node)
        [body' _] (propagate-node (:body node) inner-env)]
    [(assoc node :body body') env]))

;; ── 顶层入口 ──
(defn propagate [nodes env]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :method :record)
            (first (propagate-node node env))
            node))
        nodes))