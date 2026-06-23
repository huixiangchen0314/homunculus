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


(defrecord LetNode [bindings body bindings-count meta parent]
  p/INode
  (kind [_] :let)
  (children [_] (into (vec bindings) (if body [body] [])))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))


(defrecord FnNode [name params body meta parent]
  p/INode
  (kind [_] :fn)
  (children [_]
    (into (if name [name] [])
          (concat (vec params)
                  (if body [body] []))))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

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
  (children [_] (into (vec bindings) (if body [body] [])))
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



(defrecord SetNode [var val meta parent]
  p/INode
  (kind [_] :set!)
  (children [_] [var val])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))


(defrecord TryNode [body catches finally meta parent]
  p/INode
  (kind [_] :try)
  ;; 直接子节点：body（单个节点，可能为 DoNode）、每个 catch、finally（可选）
  (children [_] (into (if body [body] [])
                      (concat catches
                              (if finally [finally] []))))
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))


(defrecord CatchNode [class sym body meta parent]
  p/INode
  (kind [_] :catch)
  (children [_] (into [class sym] body))
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

(defrecord NsNode [name docstring attr-map references meta parent]
  p/INode
  (kind [_] :ns)
  (children [_] [])
  (node-meta [_] meta)
  (parent [_] parent)
  (set-parent [this p] (assoc this :parent p)))

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
  (kind     [this] :record)
  (children [this]
    (concat
      ;; 1) 字段默认值表达式（如 (defrecord Foo [x 42]) 中的 42）
      (keep :init fields)
      ;; 2) 协议方法体
      (mapcat (fn [proto]
                (mapcat (fn [method]
                          (if-let [body (:body method)]
                            [body]
                            []))
                        (:methods proto)))
              protocols)))
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

;; ProtocolNode: 表示 defprotocol 定义
;; funcs 示例: [{:name draw
;;               :params [ ;; 不包括this
;;                        {:name x, :meta nil}]
;;               :ret :nil
;;               :meta nil}]
(defrecord ProtocolNode [name funcs meta parent]
  p/INode
  (kind       [this] :protocol)
  (children   [this] [])   ;; funcs 是函数签名列表，不是 INode 节点
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))

;; :关键字 表示属性访问，不支持设置
;; .xyz 表示方法调用
;; 不支持Clojure 风格 assoc 设置
(defrecord MemberAccessNode [target accessor args meta parent]
  p/INode
  (kind       [this] :member-access)
  (children   [this] (into [target] args))
  (node-meta  [this] meta)
  (parent     [this] parent)
  (set-parent [this p] (assoc this :parent p)))
