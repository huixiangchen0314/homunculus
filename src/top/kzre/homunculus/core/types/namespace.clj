(ns top.kzre.homunculus.core.types.namespace
  "对命名空间节点的数据处理工具。仅提供业务逻辑，基础访问器由 ir2.node 提供。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]))

(defn ns-dependency-syms
  "从 ns 节点中提取所有被 require 的外部命名空间符号。
   忽略 :as 别名和 :refer 子句，不包含当前命名空间自身。"
  [ns-node]
  (let [self   (n/namespace-name ns-node)
        refs   (n/namespace-references ns-node)]
    (->> refs
         (mapcat (fn [ref]
                   (let [ns-sym (if (sequential? ref) (first ref) ref)]
                     (when (and (symbol? ns-sym)
                                (not= ns-sym self))
                       [ns-sym]))))
         (distinct)
         (vec))))

(defn ns-reference-aliases
  "从 ns 节点中提取 :require 的别名映射，返回 {alias-sym -> full-ns-sym}。
   references 已解析为向量，元素为符号或 [ns-sym :as alias] 形式。"
  [ns-node]
  (let [refs (n/namespace-references ns-node)]
    (into {}
          (keep (fn [ref]
                  (when (and (sequential? ref)
                             (= (count ref) 3)
                             (symbol? (first ref))
                             (= :as (second ref))
                             (symbol? (nth ref 2)))
                    [(nth ref 2) (first ref)]))
                refs))))