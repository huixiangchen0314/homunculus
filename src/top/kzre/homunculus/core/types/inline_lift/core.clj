(ns top.kzre.homunculus.core.types.inline-lift.core
  "IR2 inline/lift pass：内联或提升直接调用的 lambda。多方法递归遍历。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.free-vars :as free-vars]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.subst :as subst]))

(defmulti walk
          "递归处理节点，内联/提升 lambda 调用。返回更新后的节点。"
          (fn [node config lifted] (ir2p/kind node)))

(defn transform
  "处理单个 IR2 根节点，返回 [新根 新顶层定义列表]。"
  [ir2-root config]
  (let [lifted (atom [])
        new-root (walk ir2-root config lifted)]
    [new-root @lifted]))

(defn eliminate-closures
  "处理根节点列表，将所有新产生的顶层定义合并到结果中。"
  [ir2-roots config]
  (let [results (map #(transform % config) ir2-roots)
        new-roots (mapcat (fn [[root defs]] (cons root defs)) results)]
    new-roots))