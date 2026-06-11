(ns top.kzre.homunculus.core.ir2.model
  "IR2 语言无关 AST 的节点记录定义。所有节点实现 INode 协议。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as p]))

(defrecord LiteralNode [val attrs meta children parent]
  p/INode
  (kind [_] :literal)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VariableNode [name attrs meta children parent]
  p/INode
  (kind [_] :variable)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CallNode [fn args attrs meta children parent]
  p/INode
  (kind [_] :call)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord IfNode [test then else attrs meta children parent]
  p/INode
  (kind [_] :if)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord BlockNode [exprs attrs meta children parent]
  p/INode
  (kind [_] :block)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LetNode [bindings body attrs meta children parent]
  p/INode
  (kind [_] :let)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LoopNode [bindings body attrs meta children parent]
  p/INode
  (kind [_] :loop)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord RecurNode [args attrs meta children parent]
  p/INode
  (kind [_] :recur)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LambdaNode [params body captures fn-name attrs meta children parent]
  p/INode
  (kind [_] :lambda)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord DefineNode [name val doc attrs meta children parent]
  p/INode
  (kind [_] :define)
  (children [_] children)          ;; children 仅包含 val（如果有）
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VectorNode [items attrs meta children parent]
  p/INode
  (kind [_] :vector)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord MapNode [kvs attrs meta children parent]
  p/INode
  (kind [_] :map)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord TryNode [body catches finally attrs meta children parent]
  p/INode
  (kind [_] :try)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CatchNode [class sym body attrs meta children parent]
  p/INode
  (kind [_] :catch)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ThrowNode [expr attrs meta children parent]
  p/INode
  (kind [_] :throw)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord AssignNode [var val attrs meta children parent]
  p/INode
  (kind [_] :assign)
  (children [_] children)
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))