(ns top.kzre.homunculus.core.types.fold.fold
  "常量折叠 Pass：递归遍历 IR2 树，利用后端实现的 IFolder 协议进行常量折叠。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.fold.protocol :as p]))

(defmulti fold-node (fn [node _folder _context] (n/kind node)))


;; ── 调用节点：先递归处理子节点，再尝试折叠 ──
(defmethod fold-node :call [node folder context]
  (let [new-fn  (fold-node (n/call-fn node) folder context)
        new-args (mapv #(fold-node % folder context) (n/call-args node))
        temp-node (n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))]
    (or (p/fold-node folder temp-node context)
        temp-node)))


(defmethod fold-node :default [node folder context]
  (first (ir2p/reduce-children node
                               (fn [child ctx] [(fold-node child folder ctx) ctx])
                               context)))

;; ── 入口 ──
(defn fold
  "进行一趟代码折叠，折叠是前向的，不需要积累上下文."
  [ir2-roots folder context]
  (mapv #(fold-node % folder context) ir2-roots))