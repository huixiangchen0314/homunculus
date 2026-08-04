(ns top.kzre.homunculus.core.types.fold.mutable
  "可变性分析：在传播前标记变量是否可变。
   默认 :mutable false，通过扫描赋值和循环变量覆盖为 true。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.model :as p]))

(defn- mark-var [node mutable?]
  (n/make-variable (n/var-name node)
                   (assoc (n/attrs node) :mutable mutable?)
                   (n/node-meta node)))

(defmulti mutable-node (fn [node _env] (n/kind node)))

(defmethod mutable-node :variable [node env]
  (if (contains? env (n/var-name node))
    [(mark-var node true) env]
    [node env]))

(defmethod mutable-node :let [node env]
  (let [bindings (n/let-bindings node)           ;; Binding 向量
        ;; 初始处理：值表达式递归，变量暂标记为不可变
        [new-bindings ctx]
        (reduce (fn [[bnds env] b]
                  (let [var-node (:var b)
                        val-node (:val b)
                        [new-val env1] (mutable-node val-node env)
                        ;; 标记变量为不可变，重建绑定
                        new-b (assoc b :var (mark-var var-node false) :val new-val)]
                    [(conj bnds new-b) env1]))
                [[] env]
                bindings)
        ;; 分析 body，获取可变变量集合
        [new-body env2] (mutable-node (n/let-body node) ctx)
        mutable-vars env2                         ;; 变量名集合
        ;; 根据可变集合修正绑定变量的 :mutable 标记
        corrected-bindings
        (mapv (fn [b]
                (let [var-node (:var b)]
                  (if (contains? mutable-vars (:name var-node))
                    (assoc b :var (mark-var var-node true))   ;; 重新标记为可变
                    b)))
              new-bindings)]
    [(n/make-let corrected-bindings new-body (n/attrs node) (n/node-meta node))
     env2]))

(defmethod mutable-node :loop [node env]
  (let [bindings (n/loop-bindings node)               ;; 现在是 Binding 向量
        new-bindings (mapv (fn [b]
                             ;; 循环变量标记为可变
                             (assoc b :var (mark-var (:var b) true)))
                           bindings)
        ;; 收集循环变量名，加入环境
        loop-vars (set (map #(:name (:var %)) bindings))
        inner-env (into env loop-vars)
        [new-body _] (mutable-node (n/loop-body node) inner-env)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node))
     env]))

(defmethod mutable-node :define [node env]
  (if-let [val (n/define-val node)]
    (let [[new-val env1] (mutable-node val env)]
      [(n/make-define (n/define-name node) new-val (n/define-docstring node)
                      (n/attrs node) (n/node-meta node) )
       env1])
    [node env]))

(defmethod mutable-node :assign [node env]
  ;; 赋值左侧变量加入可变集合
  (let [new-var (n/assign-var node)
        [new-val env1] (mutable-node (n/assign-val node) env)
        env2 (if (n/variable-node? new-var)
               (conj env (n/var-name new-var))
               env1)]
    [(n/make-assign (mark-var new-var true) new-val
                    (n/attrs node) (n/node-meta node) )
     env2]))



(defmethod mutable-node :if [node env]
  ;; 先分析 then / else 分支，合并其可变变量集，再用该集合去分析 test
  (let [[then env-then] (mutable-node (n/if-then node) env)
        [else env-else] (if-let [e (n/if-else node)]
                          (mutable-node e env)
                          [nil env])
        ;; 合并两个分支的可变变量（取并集）
        merged-env (into env-then env-else)
        [test env-test] (mutable-node (n/if-test node) merged-env)]
    [(n/make-if test then else (n/attrs node) (n/node-meta node))
     env-test]))

(defmethod mutable-node :while [node env]
  ;; 先分析 body，收集其中被赋值的变量，再用更新后的 env 分析 test
  (let [[body env1] (mutable-node (n/while-body node) env)
        [test env2] (mutable-node (n/while-test node) env1)]
    [(n/make-while test body (n/attrs node) (n/node-meta node) )
     env2]))


(defmethod mutable-node :lambda [node env]
  (let [params (n/lambda-params node)
        [new-body _] (mutable-node (n/lambda-body node) env)]
    [(n/make-lambda params new-body (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node))
     env]))


(defmethod mutable-node :default [node env]
  (when node
    (p/reduce-children node
                       (fn [child e] (mutable-node child e))
                       env)))


(defn analyze [nodes]
  (mapv #(first (mutable-node % #{})) nodes))