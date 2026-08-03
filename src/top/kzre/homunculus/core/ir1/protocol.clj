(ns top.kzre.homunculus.core.ir1.protocol
  "IR1 节点协议。所有节点均为记录，支持 parent 指针。")

(defprotocol INode
  (kind       [this] "返回节点类型关键字")
  (node-meta  [this] "返回元数据 map")
  )
