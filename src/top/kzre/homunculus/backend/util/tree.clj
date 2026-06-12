(ns top.kzre.homunculus.backend.util.tree
  "IR 树遍历与查询工具。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn children
  "返回节点的子节点列表。"
  [node]
  (if (satisfies? ir2p/INode node)
    (ir2p/children node)
    []))

(defn walk
  "深度优先遍历节点及其子树，对每个节点调用 f。"
  [f node]
  (f node)
  (doseq [c (children node)]
    (walk f c)))

(defn collect-of-kind
  "收集子树中所有指定 :kind 的节点。"
  [kind node]
  (let [result (atom [])]
    (walk (fn [n]
            (when (and (satisfies? ir2p/INode n)
                       (= (ir2p/kind n) kind))
              (swap! result conj n)))
          node)
    @result))

(defn collect-vars
  "收集子树中所有变量名（:variable 节点的 :name）。"
  [node]
  (let [result (atom #{})]
    (walk (fn [n]
            (when (and (satisfies? ir2p/INode n)
                       (= (ir2p/kind n) :variable))
              (swap! result conj (:name n))))
          node)
    @result))