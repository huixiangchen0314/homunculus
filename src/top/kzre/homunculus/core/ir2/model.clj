(ns top.kzre.homunculus.core.ir2.model
  "IR2 语言无关 AST 的节点记录定义。所有节点实现 INode 协议。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as p]))


(defrecord LambdaNode [params body captures fn-name attrs meta  parent]
  p/INode
  (kind [_] :lambda)
  ;; params 是 VariableNode 列表，body 是子节点
  (children [_] (into (vec params) [body]))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LiteralNode [val attrs meta parent]
  p/INode
  (kind [_] :literal)
  ;; 字面量没有子节点
  (children [_] [])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VariableNode [name attrs meta  parent]
  p/INode
  (kind [_] :variable)
  (children [_] [])   ; 明确返回空，忽略构造时传入的 children 字段
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CallNode [fn args attrs meta  parent]
  p/INode
  (kind [_] :call)
  ;; children = [fn] + args（过滤 nil）
  (children [_] (into (if fn [fn] []) (remove nil? args)))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord IfNode [test then else attrs meta  parent]
  p/INode
  (kind [_] :if)
  (children [_] (into [test then]
                      (if else [else] [])))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord BlockNode [exprs attrs meta  parent]
  p/INode
  (kind [_] :block)
  (children [_] (vec exprs))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LetNode [bindings body attrs meta  parent]
  p/INode
  (kind [_] :let)
  ;; children = 每个绑定的 var 和 val（都是 INode） + body
  (children [_] (into (mapcat (fn [[v e]] [v e]) bindings)
                      [body]))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord LoopNode [bindings body attrs meta  parent]
  p/INode
  (kind [_] :loop)
  (children [_] (into (mapcat (fn [[v e]] [v e]) bindings)
                      [body]))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord RecurNode [args attrs meta parent]
  p/INode
  (kind [_] :recur)
  (children [_] (vec args))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))






(defrecord DefineNode [name val doc attrs meta  parent]
  p/INode
  (kind [_] :define)
  ;; val 通常是一个 LambdaNode，加入 children
  (children [_] (if val [val] []))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord VectorNode [items attrs meta  parent]
  p/INode
  (kind [_] :vector)
  (children [_] (vec items))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord MapNode [kvs attrs meta  parent]
  p/INode
  (kind [_] :map)
  ;; kvs 是交替的键值对，全都是 INode
  (children [_] (vec kvs))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord TryNode [body catches finally attrs meta parent]
  p/INode
  (kind [_] :try)
  ;; children = body + catches + finally（body 是单个节点，可能为 BlockNode）
  (children [_] (into (if body [body] [])
                      (concat catches
                              (if finally [finally] []))))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord CatchNode [class sym body attrs meta parent]
  p/INode
  (kind [_] :catch)
  ;; body 是向量，直接展开
  (children [_] (into [class sym] body))
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ThrowNode [expr attrs meta  parent]
  p/INode
  (kind [_] :throw)
  (children [_] [expr])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord AssignNode [var val attrs meta  parent]
  p/INode
  (kind [_] :assign)
  (children [_] [var val])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord WhileNode [test body attrs meta  parent]
  p/INode
  (kind [_] :while)
  (children [_] [test body])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ConvertNode [expr src-ty dst-ty cost attrs meta parent]
  p/INode
  (kind [_] :convert)
  (children [_] [expr])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))


(defrecord NsNode [name docstring attr-map references attrs meta parent]
  p/INode
  (kind [_] :ns)
  ;; 命名空间声明本身没有子节点
  (children [_] [])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ── 记录定义节点 ──
(defrecord RecordNode [name fields protocols attrs meta parent]
  p/INode
  (kind [_] :record)
  ;; 子节点 = 所有字段的初始化表达式（过滤 nil）
  (children [_] (keep :init fields))                        ;; protocols 方法体被提升为全局函数定义了
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ── 协议定义节点 ──
(defrecord ProtocolNode [name funcs attrs meta parent]   ;; method-sigs → funcs
  p/INode
  (kind [_] :protocol)
  (children [_] [])
  (attrs [_] attrs)
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ── 成员访问节点 ──
(defrecord MemberAccessNode [target accessor args meta parent]
  p/INode
  (kind       [this] :member-access)
  (children   [this] (into [target] args))
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))


;; 特殊形式(非面向用户) =========================================================

;; 数组节点，可以把向量类型用命令式操作
(defrecord NewArrayNode [size meta parent]
  p/INode
  (kind       [this] :new-array)
  (children   [this] [])
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord AGetNode [target idx meta parent]
  p/INode
  (kind       [this] :aget)
  (children   [this] [target idx])
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ASetNode [target idx val meta parent]
  p/INode
  (kind       [this] :aset)
  (children   [this] [target idx val])
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ALengthNode [target meta parent]
  p/INode
  (kind       [this] :alength)
  (children   [this] [target])
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

(defrecord ASliceNode [target start end meta parent]
  p/INode
  (kind       [this] :alength)
  (children   [this] [target start end])
  (attrs      [this] {})
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

