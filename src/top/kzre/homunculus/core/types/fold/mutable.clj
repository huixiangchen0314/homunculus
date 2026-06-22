(ns top.kzre.homunculus.core.types.fold.mutable
  "可变性分析：在传播前标记变量是否可变。
   默认 :mutable false，通过扫描赋值和循环变量覆盖为 true。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]))

(defn- mark-var [node mutable?]
  (n/make-variable (n/var-name node)
                   (assoc (n/attrs node) :mutable mutable?)
                   (n/node-meta node)
                   (n/parent node)))

(defmulti mutable-node (fn [node _env] (n/kind node)))

(defmethod mutable-node :literal [node env] [node env])
(defmethod mutable-node :variable [node env]
  (if (contains? env (n/var-name node))
    [(mark-var node true) env]
    [node env]))

(defmethod mutable-node :let [node env]
  (let [bindings (n/let-bindings node)
        ;; 初始处理，变量暂标记为不可变
        [new-bindings ctx]
        (reduce (fn [[bnds env] [var val]]
                  (let [[new-val env1] (mutable-node val env)]
                    [(conj bnds [(mark-var var false) new-val]) env1]))
                [[] env]
                bindings)
        [new-body env2] (mutable-node (n/let-body node) ctx)
        ;; ★ 从 body 分析结果 env2 中获取可变变量名集合，修正绑定变量的 :mutable 标记
        mutable-vars env2                       ;; env2 是一个 set，元素为变量名
        corrected-bindings (mapv (fn [[var val]]
                                   (if (contains? mutable-vars (n/var-name var))
                                     [(mark-var var true) val]   ;; 重新标记为可变
                                     [var val]))
                                 new-bindings)]
    [(n/make-let corrected-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))
     env2]))

(defmethod mutable-node :loop [node env]
  (let [bindings (n/loop-bindings node)
        ;; 循环变量标记为可变
        new-bindings (mapv (fn [[var val]] [(mark-var var true) val]) bindings)
        ;; 收集循环变量名，在循环体内这些变量是可变的
        loop-vars (set (map (fn [[v]] (n/var-name v)) bindings))
        inner-env (into env loop-vars)
        [new-body _] (mutable-node (n/loop-body node) inner-env)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))
     env]))

(defmethod mutable-node :define [node env]
  (if-let [val (n/define-val node)]
    (let [[new-val env1] (mutable-node val env)]
      [(n/make-define (n/define-name node) new-val (n/define-doc node)
                      (n/attrs node) (n/node-meta node) (n/parent node))
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
                    (n/attrs node) (n/node-meta node) (n/parent node))
     env2]))

;; 其他节点递归子节点，环境传递
(defmethod mutable-node :call [node env]
  (let [[new-fn env1] (mutable-node (n/call-fn node) env)
        [new-args env2]
        (reduce (fn [[args e] arg]
                  (let [[a e1] (mutable-node arg e)]
                    [(conj args a) e1]))
                [[] env1]
                (n/call-args node))]
    [(n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))
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
    [(n/make-if test then else (n/attrs node) (n/node-meta node) (n/parent node))
     env-test]))

(defmethod mutable-node :while [node env]
  ;; 先分析 body，收集其中被赋值的变量，再用更新后的 env 分析 test
  (let [[body env1] (mutable-node (n/while-body node) env)
        [test env2] (mutable-node (n/while-test node) env1)]
    [(n/make-while test body (n/attrs node) (n/node-meta node) (n/parent node))
     env2]))

(defmethod mutable-node :block [node env]
  (let [[exprs env1]
        (reduce (fn [[exps e] expr]
                  (let [[ex e1] (mutable-node expr e)]
                    [(conj exps ex) e1]))
                [[] env]
                (n/block-exprs node))]
    [(n/make-block exprs (n/attrs node) (n/node-meta node) (n/parent node))
     env1]))

(defmethod mutable-node :lambda [node env]
  (let [params (n/lambda-params node)
        [new-body _] (mutable-node (n/lambda-body node) env)]
    [(n/make-lambda params new-body (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     env]))

;; 数组节点 size 不变，但 target/idx/val 需要递归
(defmethod mutable-node :new-array [node env]
  (let [[size env1] (mutable-node (n/new-array-size node) env)]
    [(n/make-new-array size (n/node-meta node) (n/parent node)) env1]))
(defmethod mutable-node :aget [node env]
  (let [[target env1] (mutable-node (n/aget-target node) env)
        [idx env2] (mutable-node (n/aget-idx node) env1)]
    [(n/make-aget target idx (n/node-meta node) (n/parent node)) env2]))
(defmethod mutable-node :aset [node env]
  (let [[target env1] (mutable-node (n/aset-target node) env)
        [idx env2] (mutable-node (n/aset-idx node) env1)
        [val env3] (mutable-node (n/aset-val node) env2)]
    [(n/make-aset target idx val (n/node-meta node) (n/parent node)) env3]))
(defmethod mutable-node :alength [node env]
  (let [[target env1] (mutable-node (n/alength-target node) env)]
    [(n/make-alength target (n/node-meta node) (n/parent node)) env1]))

(defmethod mutable-node :default [node env] [node env])

(defn analyze [ir2-roots]
  (mapv #(first (mutable-node % #{})) ir2-roots))