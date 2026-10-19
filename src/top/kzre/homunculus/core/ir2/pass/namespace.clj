(ns top.kzre.homunculus.core.ir2.pass.namespace
  "对命名空间节点的数据处理工具。仅提供业务逻辑，基础访问器由 ir2.node 提供。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]))

(defn ns-dependency-syms
  "从 ns 节点中提取所有被 require 的外部命名空间符号。
   忽略 :as 别名和 :refer 子句，不包含当前命名空间自身。"
  [ns-node]
  (let [self   (n/namespace-name ns-node)
        refs   (n/namespace-requires ns-node)]
    (->> refs
         (mapcat (fn [ref]
                   (let [ns-sym (if (sequential? ref) (first ref) ref)]
                     (when (and (symbol? ns-sym)
                                (not= ns-sym self))
                       [ns-sym]))))
         (distinct)
         (vec))))

(defn ns-exported-syms
  "从全局符号表中提取命名空间 ns-sym 的所有公开符号，
   返回 {short-name -> fully-qualified-sym} 映射。"
  [symbol-table ns-sym]
  (let [ns-str (str ns-sym)]
    (into {}
          (keep (fn [[sym entry]]
                  (when (and (symbol? sym)
                             (= (namespace sym) ns-str)
                             (not (:private (meta entry))))
                    [(symbol (name sym)) sym])))
          symbol-table)))

(defn- refer-syms
  "从 ns-exported-syms 结果里过滤出 refer-syms 指定的符号。

   只保留同时出现在 ns-sym 导出表和 refer-syms 里的符号——
   refer 引用了 ns-sym 里不存在的符号，不在此处报错（Clojure
   本身会在加载时报错），此处只负责别名映射。"
  [ns-sym symbol-table refer-syms]
  (let [exported (ns-exported-syms symbol-table ns-sym)
        wanted   (set refer-syms)]
    (into {}
          (filter (fn [[short _]] (contains? wanted short)))
          exported)))

(defn- require-clause->aliases
  "解析单条 :require 子句，返回 {short-name-or-alias -> fully-qualified-sym}。

   支持形式：
   ▸ ns-sym                     —— 无别名，无贡献
   ▸ [ns :as alias]             —— alias → ns
   ▸ [ns :refer :all]           —— ns 所有导出符号
   ▸ [ns :refer [sym1 sym2]]    —— 指定导出符号
   ▸ [ns :as a :refer [...]]    —— 组合形式

   单符号或非 vector 形式的 require 不产生别名映射——只有 ns 本身
   被引用时不需要别名。"
  [req symbol-table]
  (if-not (vector? req)
    {}
    (let [ns-sym (first req)
          kv     (rest req)]
      (if-not (and (symbol? ns-sym) (even? (count kv)))
        {}
        (reduce merge {}
                (map (fn [[k v]]
                       (case k
                         :as    {v ns-sym}
                         :refer (cond
                                  (= :all v)
                                  (ns-exported-syms symbol-table ns-sym)

                                  (vector? v)
                                  (refer-syms ns-sym symbol-table v)

                                  :else nil)
                         nil))
                     (partition 2 kv)))))))

(defn ns-reference-aliases
  "从 ns 节点中提取别名映射。

   支持的 :require 子句形式：
   ▸ [ns :as alias]              —— alias → ns
   ▸ [ns :refer :all]            —— ns 所有导出符号的 short-name → fully-qualified
   ▸ [ns :refer [sym1 sym2 ...]] —— 指定导出符号的 short-name → fully-qualified
   ▸ [ns :as a :refer [...]]     —— 组合形式（同时处理）

   返回 {short-name-or-alias -> fully-qualified-sym} 映射。"
  [ns-node symbol-table]
  (let [reqs (n/namespace-requires ns-node)]
    (reduce merge {}
            (map #(require-clause->aliases % symbol-table) reqs))))