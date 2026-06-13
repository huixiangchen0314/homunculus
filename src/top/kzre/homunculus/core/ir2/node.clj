(ns top.kzre.homunculus.core.ir2.node
  "IR2 节点字段的安全访问器与构造函数。所有对节点内部关键字的直接操作都应通过此命名空间。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]))

;; ══════════════════════════════════════════════
;; 通用字段访问
;; ══════════════════════════════════════════════
(defn kind   [node] (ir2p/kind node))
(defn attrs  [node] (:attrs node))
(defn node-meta [node] (ir2p/node-meta node))
(defn parent [node] (:parent node))
(defn children [node] (ir2p/children node))

;; ══════════════════════════════════════════════
;; 类型操作
;; ══════════════════════════════════════════════
(defn type-attr
  "从 attrs 中读取 :type"
  [node]
  (get-in node [:attrs :type]))

(defn set-type-attr
  "在 attrs 中写入 :type，返回新节点。"
  [node ty]
  (assoc-in node [:attrs :type] ty))

;; ══════════════════════════════════════════════
;; VariableNode
;; ══════════════════════════════════════════════
(defn var-name [node] (:name node))

;; ══════════════════════════════════════════════
;; LiteralNode
;; ══════════════════════════════════════════════
(defn lit-val [node] (:val node))

;; ══════════════════════════════════════════════
;; CallNode
;; ══════════════════════════════════════════════
(defn call-fn   [node] (:fn node))
(defn call-args [node] (:args node))

;; ══════════════════════════════════════════════
;; IfNode
;; ══════════════════════════════════════════════
(defn if-test [node] (:test node))
(defn if-then [node] (:then node))
(defn if-else [node] (:else node))

;; ══════════════════════════════════════════════
;; BlockNode
;; ══════════════════════════════════════════════
(defn block-exprs [node] (:exprs node))

;; ══════════════════════════════════════════════
;; LetNode
;; ══════════════════════════════════════════════
(defn let-bindings [node] (:bindings node))
(defn let-body     [node] (:body node))

;; ══════════════════════════════════════════════
;; LambdaNode
;; ══════════════════════════════════════════════
(defn lambda-params   [node] (:params node))
(defn lambda-body     [node] (:body node))
(defn lambda-captures [node] (:captures node))
(defn lambda-fn-name  [node] (:fn-name node))

;; ══════════════════════════════════════════════
;; DefineNode
;; ══════════════════════════════════════════════
(defn define-name [node] (:name node))
(defn define-val  [node] (:val node))
(defn define-doc  [node] (:doc node))

;; ══════════════════════════════════════════════
;; LoopNode
;; ══════════════════════════════════════════════
(defn loop-bindings [node] (:bindings node))
(defn loop-body     [node] (:body node))

;; ══════════════════════════════════════════════
;; RecurNode
;; ══════════════════════════════════════════════
(defn recur-args [node] (:args node))

;; ══════════════════════════════════════════════
;; WhileNode
;; ══════════════════════════════════════════════
(defn while-test [node] (:test node))
(defn while-body [node] (:body node))

;; ══════════════════════════════════════════════
;; AssignNode
;; ══════════════════════════════════════════════
(defn assign-var [node] (:var node))
(defn assign-val [node] (:val node))

;; ══════════════════════════════════════════════
;; TryNode
;; ══════════════════════════════════════════════
(defn try-body    [node] (:body node))
(defn try-catches [node] (:catches node))
(defn try-finally [node] (:finally node))

;; ══════════════════════════════════════════════
;; CatchNode
;; ══════════════════════════════════════════════
(defn catch-class [node] (:class node))
(defn catch-sym   [node] (:sym node))
(defn catch-body  [node] (:body node))

;; ══════════════════════════════════════════════
;; ThrowNode
;; ══════════════════════════════════════════════
(defn throw-expr [node] (:expr node))

;; ══════════════════════════════════════════════
;; VectorNode
;; ══════════════════════════════════════════════
(defn vec-items [node] (:items node))

;; ══════════════════════════════════════════════
;; MapNode
;; ══════════════════════════════════════════════
(defn map-kvs [node] (:kvs node))

;; ══════════════════════════════════════════════
;; 构造函数（调用现有 model 函数，统一入口）
;; ══════════════════════════════════════════════
(defn ->literal [val attrs meta parent]
  (m/->LiteralNode val attrs meta parent))

(defn ->variable [name attrs meta parent]
  (m/->VariableNode name attrs meta parent))

(defn ->call [fn args attrs meta parent]
  (m/->CallNode fn args attrs meta parent))

(defn ->if [test then else attrs meta parent]
  (m/->IfNode test then else attrs meta parent))

(defn ->block [exprs attrs meta parent]
  (m/->BlockNode exprs attrs meta parent))

(defn ->let [bindings body attrs meta parent]
  (m/->LetNode bindings body attrs meta parent))

(defn ->lambda [params body captures fn-name attrs meta parent]
  (m/->LambdaNode params body captures fn-name attrs meta parent))

(defn ->define [name val doc attrs meta parent]
  (m/->DefineNode name val doc attrs meta parent))

(defn ->loop [bindings body attrs meta parent]
  (m/->LoopNode bindings body attrs meta parent))

(defn ->recur [args attrs meta parent]
  (m/->RecurNode args attrs meta parent))

(defn ->while [test body attrs meta parent]
  (m/->WhileNode test body attrs meta parent))

(defn ->assign [var val attrs meta parent]
  (m/->AssignNode var val attrs meta parent))

(defn ->try [body catches finally attrs meta parent]
  (m/->TryNode body catches finally attrs meta parent))

(defn ->catch [class sym body attrs meta parent]
  (m/->CatchNode class sym body attrs meta parent))

(defn ->throw [expr attrs meta parent]
  (m/->ThrowNode expr attrs meta parent))

(defn ->vector [items attrs meta parent]
  (m/->VectorNode items attrs meta parent))

(defn ->map [kvs attrs meta parent]
  (m/->MapNode kvs attrs meta parent))