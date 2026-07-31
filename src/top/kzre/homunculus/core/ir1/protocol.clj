(ns top.kzre.homunculus.core.ir1.protocol
  "IR1 节点协议。所有节点均为记录，支持 parent 指针。")

(defprotocol INode
  (kind       [this] "返回节点类型关键字")
  ;; deprecated, 按结构重建，优先于通用访问
  (children   [this] "返回直接子节点的向量（每个元素是 INode）")
  (with-children [this new-children] "用 new-children 重建节点（保留 meta 等）")
  (node-meta  [this] "返回元数据 map")
  (set-parent [this p] "设置父节点，返回新节点（递归重建）"))