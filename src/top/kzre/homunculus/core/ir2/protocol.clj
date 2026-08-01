(ns top.kzre.homunculus.core.ir2.protocol)

(defprotocol INode
  (kind       [this])
  (children   [this])
  (reduce-children [this f env])
  (attrs      [this])
  (node-meta  [this]))
