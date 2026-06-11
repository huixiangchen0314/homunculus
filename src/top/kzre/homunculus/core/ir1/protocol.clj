(ns top.kzre.homunculus.core.ir1.protocol
  "IR1 节点协议。所有节点均为记录，支持 parent 指针。")

(defprotocol INode
  (kind       [this] "返回节点类型关键字")
  (children   [this] "返回直接子节点的向量（每个元素是 INode）")
  (node-meta  [this] "返回元数据 map")
  (parent     [this] "返回父节点，根节点为 nil")
  (set-parent [this p] "设置父节点，返回新节点（递归重建）"))