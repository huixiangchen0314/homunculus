(ns top.kzre.homunculus.core.types.fold.protocol)

(defprotocol IFolder
  (fold-node [this node context]
    "尝试对 node 进行常量折叠。
     node 为 IR2 节点，context 为编译上下文。
     若成功折叠，返回一个新的 IR 节点（如字面量）；否则返回 nil。"))