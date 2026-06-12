(ns top.kzre.homunculus.core.ir1.model
  "IR1 AST 节点记录定义。所有节点实现 INode 协议，支持 parent 指针。
   不再包含 children 字段，children 通过协议方法动态返回。
   注意：多表达式体（如 let/loop/fn/try 的 body）会被展开为多个子节点。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]))

;; ── 基础节点 ──────────────────────────────
(defrecord LiteralNode [val meta parent]
  p/INode
  (kind [_] :literal)
  (children [_] [])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord SymbolNode [name meta parent]
  p/INode
  (kind [_] :symbol)
  (children [_] [])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VectorNode [items meta parent]
  p/INode
  (kind [_] :vector)
  (children [_] (vec items))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord MapNode [pairs meta parent]
  p/INode
  (kind [_] :map)
  (children [_] (vec pairs))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CallNode [op args meta parent]
  p/INode
  (kind [_] :call)
  (children [_] (into [op] args))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ── 特殊形式 ──────────────────────────────
(defrecord IfNode [test then else meta parent]
  p/INode
  (kind [_] :if)
  (children [_] (into [test then] (if else [else] [])))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord DoNode [exprs meta parent]
  p/INode
  (kind [_] :do)
  (children [_] (vec exprs))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ★ 修正：body 是向量，直接展开
(defrecord LetNode [bindings body bindings-count meta parent]
  p/INode
  (kind [_] :let)
  (children [_] (into (vec bindings) body))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ★ 修正：params 和 body 都是向量，name 可能为 nil
(defrecord FnNode [name params body meta parent]
  p/INode
  (kind [_] :fn)
  (children [_] (into (if name [name] [])
                      (concat (vec params) body)))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ★ 修正：只包含 val（IR 节点），其他字段不是节点
(defrecord DefNode [name doc attr val meta parent]
  p/INode
  (kind [_] :def)
  (children [_] (if val [val] []))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LoopNode [bindings body bindings-count meta parent]
  p/INode
  (kind [_] :loop)
  (children [_] (into (vec bindings) body))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord RecurNode [exprs meta parent]
  p/INode
  (kind [_] :recur)
  (children [_] (vec exprs))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord QuoteNode [expr meta parent]
  p/INode
  (kind [_] :quote)
  (children [_] [expr])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VarNode [var-sym meta parent]
  p/INode
  (kind [_] :var)
  (children [_] [var-sym])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ThrowNode [expr meta parent]
  p/INode
  (kind [_] :throw)
  (children [_] [expr])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord SetNode [var val meta parent]
  p/INode
  (kind [_] :set!)
  (children [_] [var val])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ★ 修正：body 和 finally 是向量，catches 是节点列表
(defrecord TryNode [body catches finally meta parent]
  p/INode
  (kind [_] :try)
  (children [_] (into (vec body)
                      (concat catches (or finally []))))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ★ 修正：body 是向量
(defrecord CatchNode [class sym body meta parent]
  p/INode
  (kind [_] :catch)
  (children [_] (into [class sym] body))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))