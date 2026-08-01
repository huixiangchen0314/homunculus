(ns top.kzre.homunculus.core.ir2.model
  "IR2 语言无关 AST 的节点记录定义。所有节点实现 INode 协议。
   使用 reduce-children，每个节点直接处理字段。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as p]))

;; ── LambdaNode ──────────────────────────────
(defrecord LambdaNode [params body captures fn-name attrs meta parent]
  p/INode
  (kind [_] :lambda)
  (children [_] (into (vec params) [body]))
  (reduce-children [this f env]
    (let [[new-params env1] (reduce (fn [[ps e] p] (let [[np e2] (f p e)] [(conj ps np) e2])) [[] env] params)
          [new-body env2] (f body env1)]
      [(assoc this :params new-params :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── LiteralNode ─────────────────────────────
(defrecord LiteralNode [val attrs meta parent]
  p/INode
  (kind [_] :literal)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── VariableNode ────────────────────────────
(defrecord VariableNode [name attrs meta parent]
  p/INode
  (kind [_] :variable)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── CallNode ────────────────────────────────
(defrecord CallNode [fn args attrs meta parent]
  p/INode
  (kind [_] :call)
  (children [_] (into (if fn [fn] []) (remove nil? args)))
  (reduce-children [this f env]
    (let [[new-fn env1] (if fn (f fn env) [nil env])
          [new-args env2] (reduce (clojure.core/fn [[as e] a] (let [[na e2] (f a e)] [(conj as na) e2])) [[] env1] args)]
      [(assoc this :fn new-fn :args new-args) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── IfNode ──────────────────────────────────
(defrecord IfNode [test then else attrs meta parent]
  p/INode
  (kind [_] :if)
  (children [_] (into [test then] (if else [else] [])))
  (reduce-children [this f env]
    (let [[new-test env1] (f test env)
          [new-then env2] (f then env1)
          [new-else env3] (if else (f else env2) [nil env2])]
      [(assoc this :test new-test :then new-then :else new-else) env3]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── BlockNode ───────────────────────────────
(defrecord BlockNode [exprs attrs meta parent]
  p/INode
  (kind [_] :block)
  (children [_] (vec exprs))
  (reduce-children [this f env]
    (let [[new-exprs env1] (reduce (fn [[es e] expr] (let [[ne e2] (f expr e)] [(conj es ne) e2])) [[] env] exprs)]
      [(assoc this :exprs new-exprs) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── LetNode ─────────────────────────────────
(defrecord LetNode [bindings body attrs meta parent]
  p/INode
  (kind [_] :let)
  (children [_] (into (mapcat (fn [[v e]] [v e]) bindings) [body]))
  (reduce-children [this f env]
    (let [[new-bindings env1]
          (reduce (fn [[bnds e] [v val]]
                    (let [[new-val e1] (f val e)
                          [new-v e2] (f v e1)]
                      [(conj bnds [new-v new-val]) e2]))
                  [[] env] bindings)
          [new-body env2] (f body env1)]
      [(assoc this :bindings new-bindings :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── LoopNode ────────────────────────────────
(defrecord LoopNode [bindings body attrs meta parent]
  p/INode
  (kind [_] :loop)
  (children [_] (into (mapcat (fn [[v e]] [v e]) bindings) [body]))
  (reduce-children [this f env]
    (let [[new-bindings env1]
          (reduce (fn [[bnds e] [v val]]
                    (let [[new-val e1] (f val e)
                          [new-v e2] (f v e1)]
                      [(conj bnds [new-v new-val]) e2]))
                  [[] env] bindings)
          [new-body env2] (f body env1)]
      [(assoc this :bindings new-bindings :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── RecurNode ───────────────────────────────
(defrecord RecurNode [args attrs meta parent]
  p/INode
  (kind [_] :recur)
  (children [_] (vec args))
  (reduce-children [this f env]
    (let [[new-args env1] (reduce (fn [[as e] a] (let [[na e2] (f a e)] [(conj as na) e2])) [[] env] args)]
      [(assoc this :args new-args) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── DefineNode ──────────────────────────────
(defrecord DefineNode [name val doc attrs meta parent]
  p/INode
  (kind [_] :define)
  (children [_] (if val [val] []))
  (reduce-children [this f env]
    (if val
      (let [[new-val env1] (f val env)]
        [(assoc this :val new-val) env1])
      [this env]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── VectorNode ──────────────────────────────
(defrecord VectorNode [items attrs meta parent]
  p/INode
  (kind [_] :vector)
  (children [_] (vec items))
  (reduce-children [this f env]
    (let [[new-items env1] (reduce (fn [[is e] item] (let [[ni e2] (f item e)] [(conj is ni) e2])) [[] env] items)]
      [(assoc this :items new-items) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── MapNode ─────────────────────────────────
(defrecord MapNode [kvs attrs meta parent]
  p/INode
  (kind [_] :map)
  (children [_] (vec kvs))
  (reduce-children [this f env]
    (let [[new-kvs env1] (reduce (fn [[ks e] kv] (let [[nk e2] (f kv e)] [(conj ks nk) e2])) [[] env] kvs)]
      [(assoc this :kvs new-kvs) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── TryNode ─────────────────────────────────
(defrecord TryNode [body catches finally attrs meta parent]
  p/INode
  (kind [_] :try)
  (children [_] (into (if body [body] [])
                      (concat catches
                              (if finally [finally] []))))
  (reduce-children [this f env]
    (let [[new-body env1] (if body (f body env) [nil env])
          [new-catches env2] (reduce (fn [[cs e] c] (let [[nc e2] (f c e)] [(conj cs nc) e2])) [[] env1] catches)
          [new-finally env3] (if finally (f finally env2) [nil env2])]
      [(assoc this :body new-body :catches new-catches :finally new-finally) env3]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── CatchNode ───────────────────────────────
(defrecord CatchNode [class sym body attrs meta parent]
  p/INode
  (kind [_] :catch)
  (children [_] (into [class sym] body))
  (reduce-children [this f env]
    (let [[new-class env1] (f class env)
          [new-sym env2] (f sym env1)
          [new-body env3] (reduce (fn [[es e] expr] (let [[ne e2] (f expr e)] [(conj es ne) e2])) [[] env2] body)]
      [(assoc this :class new-class :sym new-sym :body new-body) env3]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── ThrowNode ───────────────────────────────
(defrecord ThrowNode [expr attrs meta parent]
  p/INode
  (kind [_] :throw)
  (children [_] [expr])
  (reduce-children [this f env]
    (let [[new-expr env1] (f expr env)]
      [(assoc this :expr new-expr) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── AssignNode ──────────────────────────────
(defrecord AssignNode [var val attrs meta parent]
  p/INode
  (kind [_] :assign)
  (children [_] [var val])
  (reduce-children [this f env]
    (let [[new-var env1] (f var env)
          [new-val env2] (f val env1)]
      [(assoc this :var new-var :val new-val) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── WhileNode ───────────────────────────────
(defrecord WhileNode [test body attrs meta parent]
  p/INode
  (kind [_] :while)
  (children [_] [test body])
  (reduce-children [this f env]
    (let [[new-test env1] (f test env)
          [new-body env2] (f body env1)]
      [(assoc this :test new-test :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── ConvertNode ─────────────────────────────
(defrecord ConvertNode [expr src-ty dst-ty cost attrs meta parent]
  p/INode
  (kind [_] :convert)
  (children [_] [expr])
  (reduce-children [this f env]
    (let [[new-expr env1] (f expr env)]
      [(assoc this :expr new-expr) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── NsNode ──────────────────────────────────
(defrecord NsNode [name docstring attr-map references attrs meta parent]
  p/INode
  (kind [_] :ns)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── RecordNode ──────────────────────────────
(defrecord RecordNode [name fields protocols attrs meta parent]
  p/INode
  (kind [_] :record)
  (children [_] (keep :init fields))
  (reduce-children [this f env]
    (let [inits (keep :init fields)
          [new-inits env1] (reduce (fn [[is e] init] (let [[ni e2] (f init e)] [(conj is ni) e2])) [[] env] inits)
          new-fields (loop [i 0, fs fields, res []]
                       (if (empty? fs)
                         res
                         (let [fld (first fs)
                               init (:init fld)]
                           (if init
                             (recur (inc i) (rest fs) (conj res (assoc fld :init (nth new-inits i))))
                             (recur i (rest fs) (conj res fld))))))]
      [(assoc this :fields new-fields) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── ProtocolNode ────────────────────────────
(defrecord ProtocolNode [name funcs attrs meta parent]
  p/INode
  (kind [_] :protocol)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── MemberAccessNode ────────────────────────
(defrecord MemberAccessNode [target accessor args attrs meta parent]
  p/INode
  (kind [_] :member-access)
  (children [_] (into [target] args))
  (reduce-children [this f env]
    (let [[new-target env1] (f target env)
          [new-args env2] (reduce (fn [[as e] a] (let [[na e2] (f a e)] [(conj as na) e2])) [[] env1] args)]
      [(assoc this :target new-target :args new-args) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── NewArrayNode ────────────────────────────
(defrecord NewArrayNode [size attrs meta parent]
  p/INode
  (kind [_] :new-array)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── AGetNode ────────────────────────────────
(defrecord AGetNode [target idx attrs meta parent]
  p/INode
  (kind [_] :aget)
  (children [_] [target idx])
  (reduce-children [this f env]
    (let [[new-target env1] (f target env)
          [new-idx env2] (f idx env1)]
      [(assoc this :target new-target :idx new-idx) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── ASetNode ────────────────────────────────
(defrecord ASetNode [target idx val attrs meta parent]
  p/INode
  (kind [_] :aset)
  (children [_] [target idx val])
  (reduce-children [this f env]
    (let [[new-target env1] (f target env)
          [new-idx env2] (f idx env1)
          [new-val env3] (f val env2)]
      [(assoc this :target new-target :idx new-idx :val new-val) env3]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── ALengthNode ─────────────────────────────
(defrecord ALengthNode [target attrs meta parent]
  p/INode
  (kind [_] :alength)
  (children [_] [target])
  (reduce-children [this f env]
    (let [[new-target env1] (f target env)]
      [(assoc this :target new-target) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)