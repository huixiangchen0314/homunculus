(ns top.kzre.homunculus.core.irstmt.pass.dce.env
  "DCE 环境的协议化实现。

   核心语义（与活跃性分析等价）：
   - use-counts[x] > 0  ⇔  x 在当前点之后仍被 use
   - def-var 清零 use-counts[x]：等价于活跃性中的 live - {x}（kill）
   - if 分支合并用 merge-max：任一路径需要就 live
   - while 不动点：body 的 live-in 累加到入口 live，保证捕获外部变量时正确

   计数归一化到 0/1 保证 while 迭代单调收敛。"
  (:require
    [top.kzre.homunculus.internal.protocol :as proto]
    [top.kzre.homunculus.core.symbol :as sym]))

(defprotocol IEnv
  (use-var    [env var-name] "增加变量的使用计数，返回新 env")
  (use-count  [env var-name] "查询变量使用计数")
  (has-use?   [env var-name] "是否曾记录过变量使用")
  (def-var    [env var-name] "记录变量定义，清零使用计数，返回新 env")
  (def-count  [env var-name] "查询变量定义计数")
  (merge-max  [env other]    "合并两个 env，同 key 取 max 并归一化"))

;; ── 内部辅助 ──
(defn- normalize-count [n]
  (if (pos? (long n)) 1 0))

(defn- normalize-counts [counts]
  (persistent!
    (reduce-kv (fn [m k v] (assoc! m k (normalize-count v)))
               (transient {})
               counts)))

(defn- merge-counts [a b]
  (normalize-counts (merge-with (fn [x y] (max (long x) (long y))) a b)))

;; ── Env 实现 ──
(defrecord Env [ctx def-counts use-counts]
  IEnv
  (use-var [_ v]
    (->Env ctx def-counts (update use-counts v (fnil inc 0))))
  (use-count [_ v] (get use-counts v 0))
  (has-use?  [_ v] (contains? use-counts v))
  (def-var   [_ v]
    (->Env ctx
           (update def-counts v (fnil inc 0))
           (assoc use-counts v 0)))
  (def-count [_ v] (get def-counts v 0))
  (merge-max [_ other]
    (->Env ctx
           (merge-counts def-counts (:def-counts other))
           (merge-counts use-counts (:use-counts other)))))

(defn make-env [ctx] (->Env ctx {} {}))

(defn same-counts?
  "两个 env 的计数是否一致（忽略 ctx）。用于 while 不动点收敛判断。"
  [e1 e2]
  (and (= (:def-counts e1) (:def-counts e2))
       (= (:use-counts e1) (:use-counts e2))))

(defn fn-fx?
  "查询函数是否有副作用。"
  [env fn-name]
  (let [tbl   (proto/symbol-table (:ctx env))
        entry (sym/lookup-sym tbl fn-name)]
    (sym/func-fx? entry)))

(defn new-defs
  "返回 a 相对 b 新增定义的变量集合（a.def-counts - b.def-counts）。
   用于 DCE 判断某语句定义了哪些变量。"
  [a b]
  (let [ac (:def-counts a)
        bc (:def-counts b)]
    (into #{}
          (filter (fn [v] (> (get ac v 0) (get bc v 0))))
          (keys ac))))