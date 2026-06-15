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

(defn call-with-fn [node fn-node]
  (assoc node :fn fn-node))

(defn call-with-args [node args]
  (assoc node :args args))

(defn call-with-children [node fn-node args]
  (-> node
      (assoc :fn fn-node)
      (assoc :args args)))

;; ══════════════════════════════════════════════
;; IfNode
;; ══════════════════════════════════════════════
;; ctor
(defn ->if [test then else attrs meta parent]
  (m/->IfNode test then else attrs meta parent))

;; accessor
(defn if-test [node] (:test node))
(defn if-then [node] (:then node))
(defn if-else [node] (:else node))

(defn if-with-test [node test] (assoc node :test test))
(defn if-with-then [node then] (assoc node :then then))
(defn if-with-else [node else] (assoc node :else else))
(defn if-with-children [node test then else]
  (assoc node :test test :then then :else else))


;; ══════════════════════════════════════════════
;; BlockNode
;; ══════════════════════════════════════════════
(defn ->block [exprs attrs meta parent]
  (m/->BlockNode exprs attrs meta parent))

(defn block-exprs [node] (:exprs node))
(defn block-with-exprs [node exprs]
  (assoc node :exprs exprs))

;; ══════════════════════════════════════════════
;; LetNode
;; ══════════════════════════════════════════════
(defn let-bindings [node] (:bindings node))
(defn let-body     [node] (:body node))
(defn let-with-bindings [node bindings]
  (assoc node :bindings bindings))

(defn let-with-body [node body]
  (assoc node :body body))

(defn let-with-children [node bindings body]
  (-> node
      (assoc :bindings bindings)
      (assoc :body body)))
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
(defn define-with-val [node val-node]
  (assoc node :val val-node))
;; ══════════════════════════════════════════════
;; LoopNode
;; ══════════════════════════════════════════════
(defn loop-bindings [node] (:bindings node))
(defn loop-body     [node] (:body node))
(defn loop-with-bindings [node bindings]
  (assoc node :bindings bindings))

(defn loop-with-body [node body]
  (assoc node :body body))

(defn loop-with-children [node bindings body]
  (-> node
      (assoc :bindings bindings)
      (assoc :body body)))
;; ══════════════════════════════════════════════
;; RecurNode
;; ══════════════════════════════════════════════
(defn recur-args [node] (:args node))

;; ══════════════════════════════════════════════
;; WhileNode
;; ══════════════════════════════════════════════
(defn while-test [node] (:test node))
(defn while-body [node] (:body node))
(defn while-with-test [node test-node]
  (assoc node :test test-node))

(defn while-with-body [node body-node]
  (assoc node :body body-node))

(defn while-with-children [node test-node body-node]
  (-> node
      (assoc :test test-node)
      (assoc :body body-node)))
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
(defn vector-items [node] (:items node))
(defn vector-with-items [node items] (assoc node :items (vec items)))
;; ══════════════════════════════════════════════
;; MapNode
;; ══════════════════════════════════════════════
(defn map-kvs [node] (:kvs node))
(defn map-with-kvs [node kvs] (assoc node :kvs (vec kvs)))
;; ══════════════════════════════════════════════
;; 构造函数（调用现有 model 函数，统一入口）
;; ══════════════════════════════════════════════
(defn ->literal [val attrs meta parent]
  (m/->LiteralNode val attrs meta parent))

(defn ->variable [name attrs meta parent]
  (m/->VariableNode name attrs meta parent))

(defn ->call [fn args attrs meta parent]
  (m/->CallNode fn args attrs meta parent))

;; ══════════════════════════════════════════════
;; ConvertNode
;; ══════════════════════════════════════════════
(defn convert-expr [node] (:expr node))
(defn convert-src-ty [node] (:src-ty node))
(defn convert-dst-ty [node] (:dst-ty node))
(defn convert-cost [node] (:cost node))

(defn convert-with-expr [node expr] (assoc node :expr expr))

(defn ->convert [expr src-ty dst-ty cost attrs meta parent]
  (m/->ConvertNode expr src-ty dst-ty cost attrs meta parent))


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

(defn define-node? [node]
  (= (some-> node ir2p/kind) :define))