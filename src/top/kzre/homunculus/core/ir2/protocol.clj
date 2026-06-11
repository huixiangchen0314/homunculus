(ns top.kzre.homunculus.core.ir2.protocol)

(defprotocol INode
  (kind       [this])
  (children   [this])
  (attrs      [this])
  (node-meta  [this])
  (parent     [this])
  (set-parent [this p]))