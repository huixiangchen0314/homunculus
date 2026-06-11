(ns top.kzre.homunculus.core.ir1.model
  "IR1 AST 节点记录定义。所有节点实现 INode 协议，支持 parent 指针。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]))

;; ── 基础节点 ──────────────────────────────
(defrecord LiteralNode [val meta children parent]
  p/INode
  (kind [_] :literal)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord SymbolNode [name meta children parent]
  p/INode
  (kind [_] :symbol)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VectorNode [items meta children parent]
  p/INode
  (kind [_] :vector)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord MapNode [pairs meta children parent]
  p/INode
  (kind [_] :map)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CallNode [op args meta children parent]
  p/INode
  (kind [_] :call)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ── 特殊形式 ──────────────────────────────
(defrecord IfNode [test then else meta children parent]
  p/INode
  (kind [_] :if)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord DoNode [exprs meta children parent]
  p/INode
  (kind [_] :do)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LetNode [bindings body bindings-count meta children parent]
  p/INode
  (kind [_] :let*)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord FnNode [name params body meta children parent]
  p/INode
  (kind [_] :fn*)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord DefNode [name doc attr val meta children parent]
  p/INode
  (kind [_] :def)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LoopNode [bindings body bindings-count meta children parent]
  p/INode
  (kind [_] :loop)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord RecurNode [exprs meta children parent]
  p/INode
  (kind [_] :recur)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord QuoteNode [expr meta children parent]
  p/INode
  (kind [_] :quote)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VarNode [var-sym meta children parent]
  p/INode
  (kind [_] :var)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ThrowNode [expr meta children parent]
  p/INode
  (kind [_] :throw)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord SetNode [var val meta children parent]
  p/INode
  (kind [_] :set!)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord TryNode [body catches finally meta children parent]
  p/INode
  (kind [_] :try)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CatchNode [class sym body meta children parent]
  p/INode
  (kind [_] :catch)
  (children [_] children)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))