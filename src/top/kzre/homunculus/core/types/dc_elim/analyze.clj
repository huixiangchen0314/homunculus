(ns top.kzre.homunculus.core.types.dc-elim.analyze
  "死代码消除的分析pass. 执行以下分析
  1. 活跃变量分析
  2. io? 副作用标记传播
  3. 单纯赋值路径分析 —— 仅预留框架，实际收集由独立 Pass 完成"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.internal.protocol :as proto]))

;; ── 环境：移除了 assign-table 字段 ──
(defrecord Env [ctx           ;; ICompileContext 实例
                living-vars   ;; 当前点之后的活跃变量集合
                io?           ;; 当前点是否包含副作用
                ])

(defn make-env [ctx]
  (->Env ctx #{} false))

(defn env-add-use [env var-name]
  (update env :living-vars conj var-name))

(defn env-kill [env var-name]
  (update env :living-vars disj var-name))

(defn env-merge-branch [env1 env2]
  (->Env (:ctx env1)
         (set/union (:living-vars env1) (:living-vars env2))
         (or (:io? env1) (:io? env2))))

;; ── 多方法：仅负责节点自身的分析动作，不遍历子节点 ──
(defmulti analyze-node (fn [node _env] (ir2/kind node)))

(defmethod analyze-node :default [node env]
  [(assoc-in node [:attrs :live-out] (:living-vars env))
   env])

(defn- should-track? [sym ctx]
  (let [tbl (proto/symbol-table ctx)
        entry (sym/lookup-sym tbl sym)]
    (or (not entry)
        (= :variable (:kind entry)))))

(defmethod analyze-node :variable [node env]
  (let [var-name (n/var-name node)
        live-out (:living-vars env)]
    (if (should-track? var-name (:ctx env))
      (let [live-in (conj live-out var-name)]
        [(-> node
             (assoc-in [:attrs :live-out] live-out)
             (assoc-in [:attrs :live-in] live-in))
         (assoc env :living-vars live-in)])
      [(-> node
           (assoc-in [:attrs :live-out] live-out)
           (assoc-in [:attrs :live-in] live-out))
       env])))

(defmethod analyze-node :call [node env]
  (let [fn-name    (n/var-name (n/call-fn node))
        symbol-tbl (proto/symbol-table (:ctx env))
        fn-entry   (when fn-name (sym/lookup-func symbol-tbl fn-name))
        io?        (:io? fn-entry)
        pure?      (:pure? fn-entry)
        fx?        (or io? (not pure?))
        new-env    (cond-> env fx? (assoc :io? true))]
    [(assoc-in node [:attrs :io?] fx?)
     new-env]))

(defmethod analyze-node :binding [node env]
  ;; 此处仅保留活跃变量分析和副作用传播，不再收集赋值表
  (let [var-node (:var node)
        env'     (if (= :variable (ir2/kind var-node))
                   (let [lhs-name (n/var-name var-node)]
                     (env-kill env lhs-name))  ;; 只杀死，不记录赋值
                   env)]
    [(-> node
         (assoc-in [:attrs :live-out] (:living-vars env))
         (assoc-in [:attrs :io?] (:io? env')))
     env']))

(defmethod analyze-node :define [node env]
  [(assoc-in node [:attrs :live-out] (:living-vars env))
   env])

;; ── 遍历控制 ──
(declare analyze-fn)

(defn analyze-fn [node env]
  (case (ir2/kind node)

    :lambda
    (let [params (n/lambda-params node)
          body   (n/lambda-body node)
          param-names (into #{} (keep #(when (= :variable (ir2/kind %)) (n/var-name %)) params))
          [new-body env-body] (analyze-fn body env)
          free-vars (set/difference (:living-vars env-body) param-names)
          env' (update env :living-vars into free-vars)]
      (analyze-node (assoc node :body new-body) env'))

    :define
    (let [val (n/define-val node)]
      (if (and val (= :lambda (ir2/kind val)))
        (let [[new-val env'] (analyze-fn val env)]
          (analyze-node (assoc node :val new-val) env'))
        (let [[new-node env'] (ir2/rreduce-children node analyze-fn env)]
          (analyze-node new-node env'))))

    :if
    (let [test (n/if-test node)
          then (n/if-then node)
          else (n/if-else node)
          [new-then env-then] (analyze-fn then env)
          [new-else env-else] (if else (analyze-fn else env) [nil env])
          env-merged (env-merge-branch env-then env-else)
          [new-test env-test] (analyze-fn test env-merged)]
      (analyze-node (assoc node :test new-test :then new-then :else new-else) env-test))

    :while
    (let [test (n/while-test node)
          body (n/while-body node)]
      (loop [env-loop (:living-vars env)
             env-out env]
        (let [[new-body env-body] (analyze-fn body (assoc env-out :living-vars env-loop))
              env-body-live (:living-vars env-body)
              [new-test env-test] (analyze-fn test (assoc env-out :living-vars env-body-live))
              new-loop-live (set/union (:living-vars env)
                                       (:living-vars env-test)
                                       env-body-live)]
          (if (= new-loop-live env-loop)
            (analyze-node (assoc node :test new-test :body new-body) env-test)
            (recur new-loop-live env-out)))))

    ;; 默认逆序遍历
    (let [[new-node env'] (ir2/rreduce-children node analyze-fn env)]
      (analyze-node new-node env'))))

;; ── 顶层入口 ──
(defn analyze-nodes [nodes ctx]
  (let [init-env (make-env ctx)
        [new-nodes _]
        (reduce (fn [[nodes' env] n]
                  (let [[n' new-env] (analyze-fn n env)]
                    [(conj nodes' n') new-env]))
                [[] init-env]
                (reverse nodes))]
    (vec (reverse new-nodes))))