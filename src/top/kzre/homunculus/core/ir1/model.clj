(ns top.kzre.homunculus.core.ir1.model
  "IR1 AST 节点记录定义。所有节点实现 INode 协议，支持 parent 指针。
   不再包含 children 字段，children 通过协议方法动态返回。
   注意：多表达式体（如 let/loop/fn/try 的 body）会被展开为多个子节点。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]))

;; ── 基础节点 ──────────────────────────────
(defrecord LiteralNode [val meta parent]
  p/INode
  (kind [_] :literal)

  (node-meta [_] meta)
  )

(defrecord SymbolNode [name meta]
  p/INode
  (kind [_] :symbol)

  (node-meta [_] meta)
  )

(defrecord VectorNode [items meta]
  p/INode
  (kind [_] :vector)

  (node-meta [_] meta)
  )

(defrecord MapNode [pairs meta parent]
  p/INode
  (kind [_] :map)

  (node-meta [_] meta)
  )

(defrecord CallNode [op args meta]
  p/INode
  (kind [_] :call)

  (node-meta [_] meta)
  )

;; ── 特殊形式 ──────────────────────────────
(defrecord IfNode [test then else meta ]
  p/INode
  (kind [_] :if)

  (node-meta [_] meta)
  )

(defrecord DoNode [exprs meta ]
  p/INode
  (kind [_] :do)

  (node-meta [_] meta)
  )


(defrecord LetNode [bindings body bindings-count meta]
  p/INode
  (kind [_] :let)

  (node-meta [_] meta)
  )


(defrecord FnNode [name params body meta ]
  p/INode
  (kind [_] :fn)
  (node-meta [_] meta)
  )

(defrecord DefNode [name doc attr val meta ]
  p/INode
  (kind [_] :def)

  (node-meta [_] meta)
  )

(defrecord LoopNode [bindings body bindings-count meta parent]
  p/INode
  (kind [_] :loop)

  (node-meta [_] meta)
  )

(defrecord RecurNode [exprs meta parent]
  p/INode
  (kind [_] :recur)

  (node-meta [_] meta)
  )

(defrecord QuoteNode [expr meta parent]
  p/INode
  (kind [_] :quote)

  (node-meta [_] meta)
  )

(defrecord VarNode [var-sym meta parent]
  p/INode
  (kind [_] :var)

  (node-meta [_] meta)
  )



(defrecord SetNode [var val meta parent]
  p/INode
  (kind [_] :set!)

  (node-meta [_] meta)
  )


(defrecord TryNode [body catches finally meta parent]
  p/INode
  (kind [_] :try)
  (node-meta [_] meta)
  )


(defrecord CatchNode [class sym body meta parent]
  p/INode
  (kind [_] :catch)

  (node-meta [_] meta)
  )

(defrecord ThrowNode [expr meta parent]
  p/INode
  (kind [_] :throw)

  (node-meta [_] meta)
  )

(defrecord NsNode [name docstring attr-map references meta parent]
  p/INode
  (kind [_] :ns)

  (node-meta [_] meta)
  )

;; RecordNode: 表示 defrecord 定义
;; name symbol?
;; field map?
;; {
;; :name :user
;; :meta nil
;; :init <expr-node>
;; }
;; protocols vector
;; [ {
;; :protocol 'top.kzre.homunculus.internal/ICompiler
;; :methods [{
;; :name 'emit
;; :params [{:name 'this, :meta nil} {:name 'context, meta: nil}...]
;; :body <block-node>
;; }]
;; }... ]
(defrecord RecordNode [name fields protocols meta parent]
  p/INode
  (kind     [_] :record)
  (node-meta  [_] meta)
  )

;; ProtocolNode: 表示 defprotocol 定义
;; funcs 示例: [{:name draw
;;               :params [ ;; 不包括this
;;                        {:name x, :meta nil}]
;;               :ret :nil
;;               :meta nil}]
(defrecord ProtocolNode [name funcs meta parent]
  p/INode
  (kind       [_] :protocol)
  (node-meta  [_] meta)
  )

;; :关键字 表示属性访问，不支持设置
;; .xyz 表示方法调用
;; 不支持Clojure 风格 assoc 设置
(defrecord MemberAccessNode [target accessor args meta parent]
  p/INode
  (kind       [_] :member-access)
  (node-meta  [_] meta)
  )

