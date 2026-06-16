(ns top.kzre.homunculus.core.types.free-vars
  "通用自由变量分析。所有节点访问均通过 ir2.node。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.set :as set]))

;; ── 收集子树内所有局部绑定 ──────────────
(defn- collect-bound
  "返回 node 子树内所有被 let/lambda/loop 引入的变量名集合。"
  [node]
  (if (satisfies? ir2p/INode node)
    (let [here (case (n/kind node)
                 :let    (into #{} (map (fn [[v _]] (n/var-name v)) (n/let-bindings node)))
                 :lambda (into #{} (map n/var-name (n/lambda-params node)))
                 :loop   (into #{} (map (fn [[v _]] (n/var-name v)) (n/loop-bindings node)))
                 #{})
          children-bounds (reduce into #{} (map collect-bound (n/children node)))]
      (into here children-bounds))
    #{}))

;; ── 收集未绑定引用 ──────────────────────
(defn- collect-free
  "在已知 bound 集合下，返回 node 子树中所有未绑定变量名。"
  [bound node]
  (if (satisfies? ir2p/INode node)
    (if (= (n/kind node) :variable)
      (if (contains? bound (n/var-name node))
        #{}
        #{(n/var-name node)})
      (reduce into #{} (map (partial collect-free bound) (n/children node))))
    #{}))

;; ── 通用入口 ────────────────────────────
(defn analyze
  "返回 node 子树中所有自由变量（未在其内部任何绑定中定义的变量名）。"
  [node]
  (let [bound (collect-bound node)
        free  (collect-free bound node)]
    free))

;; ── lambda 专用（优先使用 captures） ────
(defn free-vars-of-lambda
  "返回 lambda 节点的自由变量集合。
   优先使用 captures 字段；若为空，则基于 body AST 计算。"
  [lam]
  (if-let [caps (seq (n/lambda-captures lam))]
    (set caps)
    (let [bound (set (map n/var-name (n/lambda-params lam)))]
      (set/difference (analyze (n/lambda-body lam)) bound))))