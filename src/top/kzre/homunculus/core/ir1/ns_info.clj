(ns top.kzre.homunculus.core.ir1.ns-info
  "IR1 命名空间信息。

   ns 形式解析后的结构化数据，在 IR1 各 pass 之间传递。
   由 ir1.preprocess 产出，被 ir1.pass.expand-symbols 等消费。

   记录字段：
     :name       命名空间名（符号）
     :aliases    alias → 全限定 ns 符号
     :refers     symbol → 全限定 ns 符号（:refer [syms] 形式）
     :refer-all  需要全量 refer 的 ns 符号集合"
  (:refer-clojure :exclude [ns-name ns-aliases ns-refers]))

(defrecord NsInfo [name aliases refers refer-all])

(defn make-ns-info
  [name aliases refers refer-all]
  (->NsInfo name aliases refers refer-all))

;; ── 访问器 ──

(defn ns-name        [info] (:name info))
(defn ns-aliases     [info] (:aliases info))
(defn ns-refers      [info] (:refers info))
(defn ns-refer-all   [info] (:refer-all info))