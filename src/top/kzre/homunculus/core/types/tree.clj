(ns top.kzre.homunculus.core.types.tree
  "操作IR2 AST 的工具函数们"
  (:require
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]))


(defn replace-node
  "在 IR2 抽象语法树中递归查找与 old 节点引用相等（identical?）的节点，并用 new 节点替换之，返回更新后的树。

 参数：
   tree - 待搜索和替换的根节点（或任意子节点）。
   old  - 要替换的目标节点对象（通过引用相等比对，非值相等）。
   new  - 替换 old 的新节点对象。

 返回值：
   如果 tree 与 old 引用相等，直接返回 new。
   如果 tree 是 IR2 复合节点（满足 ir2p/INode 协议），则递归处理其所有子节点（通过 ir2p/children 获取），
   并将子节点替换后的结果通过 assoc 更新到 tree 的 :children 字段，返回新的节点对象（浅拷贝），
   保持其他属性（如 :kind、:name 等）不变。
   如果 tree 既不是 old，也不是 IR2 复合节点，则原样返回 tree。

 重要细节：
   - 该函数是纯函数，不会修改原始树，而是通过 assoc 创建新的节点对象。
   - 替换基于引用相等（identical?），仅当某个节点在内存中与 old 为同一对象时才会被替换。
     这意味着即使有两个结构完全相同的节点，只有那个被 specify 的实例会被替换。
   - 由于只更新 :children 字段，所有要被替换的节点必须是 :children 的成员（或间接成员）。
     如果某些逻辑字段（如 :call 节点的 :fn、:args）未包含在 ir2p/children 的返回值中，
     则这部分子树中的节点不会被遍历和替换，可能导致替换遗漏。
   - 若 old 不存在于 tree 中，返回的树将与原树结构相同但根节点可能被浅拷贝（如果根是 INode）。

 示例：
   ;; 替换一个特定的调用节点
   (replace-node root old-call-node inlined-expr)"
  [tree old new]
  (if (identical? tree old)
    new
    (if (satisfies? ir2p/INode tree)
      (let [new-children (mapv #(replace-node % old new) (ir2p/children tree))]
        (clojure.core/assoc tree :children new-children))
      tree)))