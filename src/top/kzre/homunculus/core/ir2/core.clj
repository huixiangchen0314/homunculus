(ns top.kzre.homunculus.core.ir2.core
  "IR2 lowering 核心调度：定义多方法 lower-ast 与入口函数。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]))

(defn ir1-meta [ir1-node] (ir1p/node-meta ir1-node))

(defmulti lower-ast
          "将 IR1 节点降低为 IR2 节点向量。"
          (fn [ir1-node _env] (ir1p/kind ir1-node)))

(defmethod lower-ast :default [node _env]
  [])

(defn lower
  "对一组 IR1 根节点执行 lowering，返回 IR2 节点向量。"
  [ir1-roots]
  (mapcat #(lower-ast % {}) ir1-roots))

(defn ->ir2
  "对单个 IR1 根节点执行 lowering，返回 IR2 节点向量。"
  [ir1-root]
  (lower-ast ir1-root {}))