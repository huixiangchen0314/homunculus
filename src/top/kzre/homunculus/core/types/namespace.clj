(ns top.kzre.homunculus.core.types.namespace
  "对命名空间节点的数据处理工具。仅提供业务逻辑，基础访问器由 ir2.node 提供。"
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.protocol :as p]))

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

(defn ns-reference-aliases
  "从 ns 节点中提取别名映射，包括 :as 别名和 :refer :all 引入的符号。
   返回 {alias-sym -> full-ns-sym} 映射。"
  [ns-node symbol-table]
  (let [refs (n/namespace-references ns-node)]
    (reduce merge {}
            (keep (fn [ref]
                    (cond
                      ;; [ns :as alias]
                      (and (vector? ref) (= (count ref) 3) (= :as (second ref)))
                      {(nth ref 2) (first ref)}

                      ;; [ns :refer :all]
                      (and (vector? ref) (= (count ref) 3) (= :refer (second ref)) (= :all (nth ref 2)))
                      (let [ns-sym (first ref)
                            all-syms (ns-exported-syms symbol-table ns-sym)]
                        all-syms)

                      :else nil))
                  refs))))
