(ns top.kzre.homunculus.internal.utils
  "编译器内部工具函数：依赖拓扑排序与循环依赖检测。"
  (:require [clojure.set :as set]))

(defn detect-cycle
  "检测依赖图中是否存在循环依赖。使用深度优先搜索（DFS）。
   graph 格式：{module-a #{module-b module-c}, ...}
   若存在循环，返回循环路径上的模块列表；否则返回 nil。"
  [graph]
  (let [WHITE 0, GRAY 1, BLACK 2
        color (atom {})
        parent (atom {})
        cycle-start (atom nil)]
    (doseq [node (keys graph)]
      (swap! color assoc node WHITE))
    (letfn [(dfs [u]
              (swap! color assoc u GRAY)
              (doseq [v (get graph u #{})]
                (let [cv (@color v WHITE)]
                  (when (= cv GRAY)
                    (reset! cycle-start v)
                    (swap! parent assoc v u))
                  (when (= cv WHITE)
                    (swap! parent assoc v u)
                    (dfs v))))
              (swap! color assoc u BLACK))]
      (doseq [node (keys graph) :when (= (@color node WHITE) WHITE)]
        (dfs node)))
    (when @cycle-start
      (loop [path [] node @cycle-start]
        (if (contains? @parent node)
          (recur (conj path node) (@parent node))
          (conj path node))))))

(defn topological-sort
  "对依赖图进行拓扑排序，返回排序后的模块列表。
   若检测到循环依赖，抛出 ex-info 异常，包含循环路径。"
  [graph]
  (if-let [cycle (detect-cycle graph)]
    (throw (ex-info "Circular dependency detected" {:cycle cycle}))
    (let [in-degree (atom {})
          zero-queue (atom clojure.lang.PersistentQueue/EMPTY)
          sorted (atom [])]
      (doseq [node (keys graph)]
        (swap! in-degree assoc node 0))
      (doseq [[u deps] graph]
        (doseq [v deps]
          (swap! in-degree update v (fnil inc 0))))
      (doseq [[node deg] @in-degree :when (zero? deg)]
        (swap! zero-queue conj node))
      (while (seq @zero-queue)
        (let [u (peek @zero-queue)]
          (swap! zero-queue pop)
          (swap! sorted conj u)
          (doseq [v (get graph u #{})]
            (swap! in-degree update v dec)
            (when (zero? (@in-degree v))
              (swap! zero-queue conj v)))))
      (if (= (count @sorted) (count (keys graph)))
        @sorted
        (throw (ex-info "Circular dependency detected" {:graph graph}))))))