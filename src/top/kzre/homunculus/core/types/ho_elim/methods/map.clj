(ns top.kzre.homunculus.core.types.ho-elim.methods.map
  "消除 (map f coll)，要求 coll 为已知大小的 VectorNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn expand-map
  [f-node coll-node]
  (let [items (node/vec-items coll-node)
        new-items (mapv (fn [item] (m/->CallNode f-node [item] nil nil nil)) items)]
    (m/->VectorNode new-items nil nil nil)))