(ns top.kzre.homunculus.core.irstmt.pass.assign-propagate.scan
  "预扫描：统计函数体内每个变量的定义次数（def-count）。

   def-count 是**函数级**属性，与作用域无关——同一变量在不同分支
   的定义累加。所以 scan 是纯收集，不维护作用域。

   定义判定：
   - var-decl 有 init    → +1（`int tmp;` 只是声明，不算定义）
   - assign 简单变量 target → +1（复合左值 `arr[i] = x` 是写内存，不算）

   与 mutable 无关：def-count 只回答\"被定义几次\"，是否可消除由 core 结合
   env/mutable-variable? 综合判断。"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(declare collect)

(defn- walk-children [node counts]
  (reduce (fn [c child] (collect child c))
          counts
          (ast/children node)))

(defn collect
  "递归收集 def-counts。nil 安全。"
  [node counts]
  (cond
    (nil? node) counts

    (= :var-decl (ast/kind node))
    (let [counts' (if (:val node)
                    (update counts (:name node) (fnil inc 0))
                    counts)]
      (walk-children node counts'))

    (= :assign (ast/kind node))
    (let [var (:var node)
          counts' (if (= :variable (ast/kind var))
                    (update counts (:name var) (fnil inc 0))
                    counts)]
      (walk-children node counts'))

    :else
    (walk-children node counts)))

(defn scan
  "扫描 :function / :method 节点，返回 def-counts map。"
  [node]
  (case (ast/kind node)
    (:function :method) (collect (:body node) {})
    {}))