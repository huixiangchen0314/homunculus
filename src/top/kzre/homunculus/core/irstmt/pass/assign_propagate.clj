(ns top.kzre.homunculus.core.irstmt.pass.assign-propagate
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defrecord Env [substs])
(defn make-env [] (->Env {}))

(defn- env-add-subst [env a b]
  (assoc-in env [:substs a] b))

(defn- env-get-subst [env a]
  (get-in env [:substs a]))

(defmulti propagate-node (fn [node _env] (ast/kind node)))

(defn walk [node env]
  (ast/reduce-children node propagate-node env))

;; ── 默认：递归所有子节点，映射向外传播 ──
(defmethod propagate-node :default [node env]
  (walk node env))

;; ── Block：块内局部 env，块外隔离 ──
(defmethod propagate-node :block [node env]
  (let [[stmts' inner-env]
        (reduce (fn [[acc e] stmt]
                  (let [[s' e'] (propagate-node stmt e)]
                    [(if s' (conj acc s') acc) e']))
                [[] env]
                (:stmts node))
        [ret' _]
        (if-let [r (:ret node)]
          (propagate-node r inner-env)
          [nil inner-env])]
    ;; 返回外层 env：块内映射（var-decl 建立的）不外泄
    [(ast/->Block stmts' ret'
                  (ast/attrs node) (ast/node-meta node))
     env]))

;; ── 变量引用：替换为映射目标 ──
(defmethod propagate-node :variable [node env]
  (let [var-name (:name node)]
    (if-let [subst (env-get-subst env var-name)]
      [(assoc node :name subst) env]
      [node env])))

;; ── 赋值：保守处理，不做消除 ──
;;
;; 消除 tmp_outer = tmp_inner 这类赋值只有在 SSA 前提（变量单次赋值）下
;; 才正确。if / while 分支合并产生的 tmp 是多次赋值，消除会导致
;; 分支里的赋值永久丢失。等引入 SSA 信息后再恢复消除优化。
(defmethod propagate-node :assign [node env]
  (let [[val-node env1] (propagate-node (:val node) env)
        [var-node env2] (propagate-node (:var node) env1)]
    [(assoc node :var var-node :val val-node) env2]))

;; ── 变量声明：init 是变量时建立映射，声明本身保留 ──
(defmethod propagate-node :var-decl [node env]
  (if-let [init (:val node)]
    (let [[init' env'] (propagate-node init env)]
      (if (= :variable (ast/kind init'))
        [(assoc node :val init')
         (env-add-subst env' (:name node) (:name init'))]
        [(assoc node :val init') env']))
    [node env]))

;; ── 函数定义：内部新环境，映射隔离 ──
(defmethod propagate-node :function [node env]
  (let [inner-env   (make-env)
        [new-body _] (propagate-node (:body node) inner-env)]
    [(assoc node :body new-body) env]))

(defn flatten-body [body]
  (if (nil? body)
    nil
    (let [stmts (:stmts body)
          ret   (:ret body)]
      (if ret
        (assoc body :stmts (conj stmts ret) :ret nil)
        body))))

;; ── while：test 用外层 env，body 用 env1 但不外泄 ──
(defmethod propagate-node :while [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        [body' _]    (propagate-node (:body node) env1)]
    [(assoc node :test test' :body (flatten-body body'))
     env1]))

;; ── if：两个分支独立从 env1 出发，不互相污染，也不泄漏到 if 之后 ──
(defmethod propagate-node :if [node env]
  (let [[test' env1] (propagate-node (:test node) env)
        [then' _]    (propagate-node (:then node) env1)   ; then 用 env1，env 丢弃
        [else' _]    (if-let [e (:else node)]
                       (propagate-node e env1)             ; else 也从 env1 出发
                       [nil env1])]
    [(assoc node :test test'
                 :then (flatten-body then')
                 :else (flatten-body else'))
     env1]))                                               ; 只返回 test 后的 env

;; ── 顶层入口 ──
(defn propagate-nodes [nodes]
  (let [env (make-env)]
    (mapv #(first (propagate-node % env)) nodes)))