(ns top.kzre.homunculus.core.ir2.pass.mark-trait
  "把节点的 trait 写入 attrs，供后续 pass 读取。

   通用 pass——不绑定任何具体 trait 消费者。
   每个 trait 的判定逻辑在 analysis.trait，这里只负责写回。

   两个入口：
   - mark      遍历全树，批处理
   - mark-node 单个节点，供产生新节点的 pass 增量使用"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.analysis.trait :as trait]))

(defn- mark-define [node]
  (-> node
      (assoc-in [:attrs :polymorphic] (trait/polymorphic? node))
      (assoc-in [:attrs :inline]      (trait/inline? node))))

(defn mark-node
  "对单个节点按 kind 分派打 trait 标记。
   未支持的 kind 原样返回——调用方不需要判断节点类型。"
  [node]
  (case (n/kind node)
    :define (mark-define node)
    node))

(defn mark
  "遍历 IR2 根节点，对每个节点打 trait 标记。"
  [ir2-roots]
  (mapv mark-node ir2-roots))