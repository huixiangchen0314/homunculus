;; ═══════════════════════════════════════════════════════
;; ir2/core.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.core
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn ir1-meta [ir1-node] (ir1p/node-meta ir1-node))

(defmulti lower-ast (fn [ir1-node env] (ir1p/kind ir1-node)))

;; 基础节点 lowering
(defmethod lower-ast :literal [node env]
  [(m/->LiteralNode (:val node) nil (ir1-meta node) nil)])

(defmethod lower-ast :symbol [node env]
  [(m/->VariableNode (name (:name node)) nil (ir1-meta node) nil)])

(defmethod lower-ast :vector [node env]
  (let [items (mapv #(first (lower-ast % env)) (ir1p/children node))]
    [(m/->VectorNode items nil (ir1-meta node) nil)]))

(defmethod lower-ast :map [node env]
  (let [pairs (:pairs node)
        kvs (mapcat (fn [[k v]]
                      [(first (lower-ast k env))
                       (first (lower-ast v env))])
                    (partition 2 pairs))]
    [(m/->MapNode kvs nil (ir1-meta node) nil)]))

(defmethod lower-ast :call [node env]
  (let [kids (ir1p/children node)
        op-node (first kids)
        arg-nodes (rest kids)
        fn-node (first (lower-ast op-node env))
        args (mapv #(first (lower-ast % env)) arg-nodes)]
    [(m/->CallNode fn-node args nil (ir1-meta node) nil)]))

;; 特殊形式占位
(defmethod lower-ast :if    [node env] nil)
(defmethod lower-ast :do    [node env] nil)
(defmethod lower-ast :let  [node env] nil)
(defmethod lower-ast :fn   [node env] nil)
(defmethod lower-ast :def   [node env] nil)
(defmethod lower-ast :loop  [node env] nil)      ;; 原为 :loop*
(defmethod lower-ast :recur [node env] nil)
(defmethod lower-ast :quote [node env] nil)
(defmethod lower-ast :try   [node env] nil)
(defmethod lower-ast :throw [node env] nil)
(defmethod lower-ast :set!  [node env] nil)
(defmethod lower-ast :var   [node env] nil)

(defn lower [ir1-roots]
  (mapcat #(lower-ast % {}) ir1-roots))