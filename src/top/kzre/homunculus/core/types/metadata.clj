(ns top.kzre.homunculus.core.types.metadata
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn node-meta
  [node]
  (when (satisfies? ir2p/INode node)
    (ir2p/node-meta node)))

(defn has-meta?
  [node k]
  (boolean (some-> node node-meta (contains? k))))

(defn get-meta
  "从节点的元数据中读取键 k 对应的值，若节点无元数据或键不存在则返回 nil。"
  [node k]
  (some-> node node-meta (get k)))