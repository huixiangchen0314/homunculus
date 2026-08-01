(ns top.kzre.homunculus.core.types.mutability.core
  "可变性分析：找出所有被 :assign 赋值的 :variable 节点，在 :attrs 中标记 {:mutable true}。
   使用 reduce-children 统一遍历，仅对 assign/variable 特判。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as p]))
;; TODO 很难证明一个变量是不可变的，这个命名空间废弃
;; ── 辅助：环境操作 ────────────────────────
(defn- add-mutable [env var-name]
  (conj (or env #{}) var-name))

;; ── 核心处理函数 ──────────────────────────
(declare walk)

(defn- annotate-fn
  "处理单个节点，返回 [new-node new-env]。
   只负责 assign 和 variable 的语义，子节点递归交给 walk。"
  [node env]
  (case (p/kind node)
    :assign
    (let [var-name (n/var-name (n/assign-var node))
          new-env (add-mutable env var-name)]
      ;; 返回 [node, new-env]，walk 会用 new-env 递归子节点
      (walk node new-env))

    :variable
    (if (contains? env (n/var-name node))
      [(n/make-variable (n/var-name node)
                        (assoc (n/attrs node) :mutable true)
                        (n/node-meta node)
                        (n/parent node))
       env]
      (walk node env))

    ;; 其他所有节点：保持不变，由 walk 递归
    (walk node env)))

(defn- walk [node env]
  (p/reduce-children node annotate-fn env))

;; ── 入口：先收集可变变量名，再标注 ─────────
(defn analyze
  "对 IR2 根节点列表进行可变性分析，返回新的 IR2 根列表。"
  [ir2-roots]
  (let [mutable-vars (atom #{})
        collect (fn collect [node]
                  (when (satisfies? p/INode node)
                    (when (= (p/kind node) :assign)
                      (swap! mutable-vars conj (n/var-name (n/assign-var node))))
                    (doseq [c (n/children node)] (collect c))))]
    (doseq [root ir2-roots] (collect root))
    (mapv #(first (annotate-fn % @mutable-vars)) ir2-roots)))