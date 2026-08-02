(ns top.kzre.homunculus.core.ir1.model
  "IR1 AST 节点记录定义。所有节点实现 INode 协议。
   不再包含 children 字段，children 通过协议方法动态返回。
   注意：多表达式体（如 let/loop/fn/try 的 body）会被展开为多个子节点。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]))

;; ── 基础节点 ──────────────────────────────
(defrecord Literal [val meta]
  p/INode
  (kind [_] :literal)
  (node-meta [_] meta)
  )

(defrecord Symbol [name meta]
  p/INode
  (kind [_] :symbol)

  (node-meta [_] meta)
  )

(defrecord Vector [items meta]
  p/INode
  (kind [_] :vector)

  (node-meta [_] meta)
  )

(defrecord Pair [key val meta]
  p/INode
  (kind [_] :pair)
  (node-meta [_] meta))

(defrecord Map [pairs meta]
  p/INode
  (kind [_] :map)
  (node-meta [_] meta)
  )

(defrecord Call [op args meta]
  p/INode
  (kind [_] :call)

  (node-meta [_] meta)
  )

;; ── 特殊形式 ──────────────────────────────
(defrecord If [test then else meta ]
  p/INode
  (kind [_] :if)

  (node-meta [_] meta)
  )

(defrecord Do [exprs meta ]
  p/INode
  (kind [_] :do)

  (node-meta [_] meta)
  )

(defrecord Binding [var val meta]
  p/INode
  (kind [_] :binding)
  (node-meta [_] meta))

(defrecord Let [bindings body bindings-count meta]
  p/INode
  (kind [_] :let)

  (node-meta [_] meta)
  )


(defrecord Fn [name params body meta ]
  p/INode
  (kind [_] :fn)
  (node-meta [_] meta)
  )

(defrecord Def [name doc attr val meta ]
  p/INode
  (kind [_] :def)

  (node-meta [_] meta)
  )

(defrecord Loop [bindings body bindings-count meta]
  p/INode
  (kind [_] :loop)
  (node-meta [_] meta)
  )

(defrecord Recur [exprs meta]
  p/INode
  (kind [_] :recur)
  (node-meta [_] meta)
  )

(defrecord Quote [expr meta]
  p/INode
  (kind [_] :quote)

  (node-meta [_] meta)
  )

(defrecord Var [var-sym meta]
  p/INode
  (kind [_] :var)

  (node-meta [_] meta)
  )



(defrecord Set [var val meta]
  p/INode
  (kind [_] :set)

  (node-meta [_] meta)
  )


(defrecord Try [body catches finally meta]
  p/INode
  (kind [_] :try)
  (node-meta [_] meta)
  )


(defrecord Catch [class sym body meta ]
  p/INode
  (kind [_] :catch)

  (node-meta [_] meta)
  )

(defrecord Throw [expr meta]
  p/INode
  (kind [_] :throw)
  (node-meta [_] meta)
  )

(defrecord Ns [name docstring attr-map references meta]
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
(defrecord Record [name fields protocols meta]
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
(defrecord Protocol [name funcs meta]
  p/INode
  (kind       [_] :protocol)
  (node-meta  [_] meta)
  )

;; :关键字 表示属性访问，不支持设置
;; .xyz 表示方法调用
;; 不支持Clojure 风格 assoc 设置
(defrecord MemberAccess [target accessor args meta]
  p/INode
  (kind       [_] :member-access)
  (node-meta  [_] meta)
  )

