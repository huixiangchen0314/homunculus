(ns top.kzre.homunculus.core.types.ho-elim.methods.reduce
  "消除 (reduce f init coll)，要求 coll 为已知大小的 VectorNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn expand-reduce
  "将 (reduce f init coll) 展开为嵌套调用，返回新的 IR2 节点。
   f-node : 函数节点（通常为 VariableNode）
   init-node : 初始值节点
   coll-node : 向量节点（VectorNode）"
  [f-node init-node coll-node]
  (let [items (node/vec-items coll-node)]
    (if (empty? items)
      init-node
      (reduce (fn [acc item]
                (m/->CallNode f-node [acc item] nil nil nil))
              init-node
              items))))