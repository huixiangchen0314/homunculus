(ns top.kzre.homunculus.core.ir2.node
  "IR2 节点字段的安全访问器、构造器与更新器。所有对节点内部关键字的直接操作都应通过此命名空间。
   统一使用 make-* 构造函数，不再使用旧的 ->* 风格。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.model :as m]))


;; 纯数据工具：将扁平绑定列表划分为 [sym val] 对
(def binding-pairs  n1/binding-pairs)
(def kv-pairs  n1/kv-pairs)
(def make-binding n1/make-binding)
(def make-pair n1/make-pair)

;; ══════════════════════════════════════════════
;; 通用字段访问
;; ══════════════════════════════════════════════
(defn kind [node]
  (when node
    (ir2p/kind node)))

(defn attrs [node]
  (when node
    (ir2p/attrs node)))

(defn node-meta [node]
  (when node
    (ir2p/node-meta node)))
(defn parent [node]
  (when node
    (:parent node)))

(defn children [node]
  (when node
    (ir2p/children node)))

;; ── 类型操作 ──
(defn type-attr [node] (get-in node [:attrs :type]))
(defn set-type-attr [node ty] (assoc-in node [:attrs :type] ty))

;; ══════════════════════════════════════════════
;; VariableNode
;; ══════════════════════════════════════════════
(defn var-name [node] (:name node))

(defn make-variable
  ([name]                      (m/->VariableNode name {} nil nil))
  ([name attrs]                (m/->VariableNode name attrs nil nil))
  ([name attrs meta]           (m/->VariableNode name attrs meta nil))
  ([name attrs meta parent]    (m/->VariableNode name attrs meta parent)))

(defn variable-with-name [node name] (assoc node :name name))
(defn variable-with-attrs [node attrs] (assoc node :attrs attrs))

;; ══════════════════════════════════════════════
;; LiteralNode
;; ══════════════════════════════════════════════
(defn lit-val [node] (:val node))

(defn make-literal
  ([val]                      (m/->LiteralNode val {} nil nil))
  ([val attrs]                (m/->LiteralNode val attrs nil nil))
  ([val attrs meta]           (m/->LiteralNode val attrs meta nil))
  ([val attrs meta parent]    (m/->LiteralNode val attrs meta parent)))

(defn literal-with-val [node val] (assoc node :val val))

;; ══════════════════════════════════════════════
;; CallNode
;; ══════════════════════════════════════════════
(defn call-fn   [node] (:fn node))
(defn call-args [node] (:args node))

(defn make-call
  ([fn args]                      (m/->CallNode fn args {} nil nil))
  ([fn args attrs]                (m/->CallNode fn args attrs nil nil))
  ([fn args attrs meta]           (m/->CallNode fn args attrs meta nil))
  ([fn args attrs meta parent]    (m/->CallNode fn args attrs meta parent)))

(defn call-with-fn   [node fn]   (assoc node :fn fn))
(defn call-with-args [node args] (assoc node :args args))
(defn call-with-children [node fn args]
  (-> node (assoc :fn fn) (assoc :args args)))

;; ══════════════════════════════════════════════
;; IfNode
;; ══════════════════════════════════════════════
(defn if-test [node] (:test node))
(defn if-then [node] (:then node))
(defn if-else [node] (:else node))

(defn make-if
  ([test then else]                      (m/->IfNode test then else {} nil nil))
  ([test then else attrs]                (m/->IfNode test then else attrs nil nil))
  ([test then else attrs meta]           (m/->IfNode test then else attrs meta nil))
  ([test then else attrs meta parent]    (m/->IfNode test then else attrs meta parent)))

(defn if-with-test  [node test] (assoc node :test test))
(defn if-with-then  [node then] (assoc node :then then))
(defn if-with-else  [node else] (assoc node :else else))
(defn if-with-children [node test then else]
  (-> node (assoc :test test) (assoc :then then) (assoc :else else)))

;; ══════════════════════════════════════════════
;; BlockNode
;; ══════════════════════════════════════════════
(defn block-exprs [node] (:exprs node))

(defn make-block
  ([exprs]                      (m/->BlockNode exprs {} nil nil))
  ([exprs attrs]                (m/->BlockNode exprs attrs nil nil))
  ([exprs attrs meta]           (m/->BlockNode exprs attrs meta nil))
  ([exprs attrs meta parent]    (m/->BlockNode exprs attrs meta parent)))

(defn block-with-exprs [node exprs] (assoc node :exprs exprs))
(defn block-node? [bode] (= (kind bode) :block))
;; ══════════════════════════════════════════════
;; LetNode
;; ══════════════════════════════════════════════
(defn let-node? [bode] (= (kind bode) :let))
(defn let-bindings [node] (:bindings node))
(defn let-body     [node] (:body node))

(defn make-let
  ([bindings body]                      (m/->LetNode bindings body {} nil nil))
  ([bindings body attrs]                (m/->LetNode bindings body attrs nil nil))
  ([bindings body attrs meta]           (m/->LetNode bindings body attrs meta nil))
  ([bindings body attrs meta parent]    (m/->LetNode bindings body attrs meta parent)))

(defn let-with-bindings [node bindings] (assoc node :bindings bindings))
(defn let-with-body     [node body]     (assoc node :body body))
(defn let-with-children [node bindings body]
  (-> node (assoc :bindings bindings) (assoc :body body)))

;; ══════════════════════════════════════════════
;; LambdaNode
;; ══════════════════════════════════════════════
(defn lambda-params   [node] (:params node))
(defn lambda-body     [node] (:body node))
(defn lambda-captures [node] (:captures node))
(defn lambda-fn-name  [node] (:fn-name node))

(defn make-lambda
  ([params body captures fn-name]                      (m/->LambdaNode params body captures fn-name {} nil nil))
  ([params body captures fn-name attrs]                (m/->LambdaNode params body captures fn-name attrs nil nil))
  ([params body captures fn-name attrs meta]           (m/->LambdaNode params body captures fn-name attrs meta nil))
  ([params body captures fn-name attrs meta parent]    (m/->LambdaNode params body captures fn-name attrs meta parent)))

(defn lambda-with-params   [node params]   (assoc node :params params))
(defn lambda-with-body     [node body]     (assoc node :body body))
(defn lambda-with-captures [node captures] (assoc node :captures captures))
(defn lambda-with-fn-name  [node fn-name]  (assoc node :fn-name fn-name))
(defn lambda-node? [node]
  (= (kind node) :lambda))

;; ══════════════════════════════════════════════
;; DefineNode
;; ══════════════════════════════════════════════
(defn define-name [node] (:name node))
(defn define-val  [node] (:val node))
(defn define-doc  [node] (:doc node))

(defn make-define
  ([name val]                      (m/->DefineNode name val nil {} nil nil))
  ([name val doc]                  (m/->DefineNode name val doc {} nil nil))
  ([name val doc attrs]            (m/->DefineNode name val doc attrs nil nil))
  ([name val doc attrs meta]       (m/->DefineNode name val doc attrs meta nil))
  ([name val doc attrs meta parent] (m/->DefineNode name val doc attrs meta parent)))

(defn define-with-val [node val] (assoc node :val val))
(defn define-with-doc [node doc] (assoc node :doc doc))

(defn define-node? [node] (= (some-> node ir2p/kind) :define))

;; ══════════════════════════════════════════════
;; LoopNode
;; ══════════════════════════════════════════════
(defn loop-bindings [node] (:bindings node))
(defn loop-body     [node] (:body node))

(defn make-loop
  ([bindings body]                      (m/->LoopNode bindings body {} nil nil))
  ([bindings body attrs]                (m/->LoopNode bindings body attrs nil nil))
  ([bindings body attrs meta]           (m/->LoopNode bindings body attrs meta nil))
  ([bindings body attrs meta parent]    (m/->LoopNode bindings body attrs meta parent)))

(defn loop-with-bindings [node bindings] (assoc node :bindings bindings))
(defn loop-with-body     [node body]     (assoc node :body body))
(defn loop-with-children [node bindings body]
  (-> node (assoc :bindings bindings) (assoc :body body)))

;; ══════════════════════════════════════════════
;; RecurNode
;; ══════════════════════════════════════════════
(defn recur-args [node] (:args node))

(defn make-recur
  ([args]                      (m/->RecurNode args {} nil nil))
  ([args attrs]                (m/->RecurNode args attrs nil nil))
  ([args attrs meta]           (m/->RecurNode args attrs meta nil))
  ([args attrs meta parent]    (m/->RecurNode args attrs meta parent)))

(defn recur-with-args [node args] (assoc node :args args))

;; ══════════════════════════════════════════════
;; WhileNode
;; ══════════════════════════════════════════════
(defn while-test [node] (:test node))
(defn while-body [node] (:body node))

(defn make-while
  ([test body]                      (m/->WhileNode test body {} nil nil))
  ([test body attrs]                (m/->WhileNode test body attrs nil nil))
  ([test body attrs meta]           (m/->WhileNode test body attrs meta nil))
  ([test body attrs meta parent]    (m/->WhileNode test body attrs meta parent)))

(defn while-with-test  [node test] (assoc node :test test))
(defn while-with-body  [node body] (assoc node :body body))
(defn while-with-children [node test body]
  (-> node (assoc :test test) (assoc :body body)))

;; ══════════════════════════════════════════════
;; AssignNode
;; ══════════════════════════════════════════════
(defn assign-var [node] (:var node))
(defn assign-val [node] (:val node))

(defn make-assign
  ([var val]                      (m/->AssignNode var val {} nil nil))
  ([var val attrs]                (m/->AssignNode var val attrs nil nil))
  ([var val attrs meta]           (m/->AssignNode var val attrs meta nil))
  ([var val attrs meta parent]    (m/->AssignNode var val attrs meta parent)))

(defn assign-with-var [node var] (assoc node :var var))
(defn assign-with-val [node val] (assoc node :val val))

;; ══════════════════════════════════════════════
;; TryNode
;; ══════════════════════════════════════════════
(defn try-body    [node] (:body node))
(defn try-catches [node] (:catches node))
(defn try-finally [node] (:finally node))

(defn make-try
  "创建 TryNode。body 为单个 INode（可能为 BlockNode），catches 为 CatchNode 列表，
   finally 为单个 INode 或 nil。"
  ([body catches finally]
   (m/->TryNode body catches finally {} nil nil))
  ([body catches finally attrs]
   (m/->TryNode body catches finally attrs nil nil))
  ([body catches finally attrs meta]
   (m/->TryNode body catches finally attrs meta nil))
  ([body catches finally attrs meta parent]
   (m/->TryNode body catches finally attrs meta parent)))

(defn try-with-body    [node body]    (assoc node :body body))
(defn try-with-catches [node catches] (assoc node :catches catches))
(defn try-with-finally [node finally] (assoc node :finally finally))

;; ══════════════════════════════════════════════
;; CatchNode
;; ══════════════════════════════════════════════
(defn catch-class [node] (:class node))
(defn catch-sym   [node] (:sym node))
(defn catch-body  [node] (:body node))

(defn make-catch
  ([class sym body]                      (m/->CatchNode class sym body {} nil nil))
  ([class sym body attrs]                (m/->CatchNode class sym body attrs nil nil))
  ([class sym body attrs meta]           (m/->CatchNode class sym body attrs meta nil))
  ([class sym body attrs meta parent]    (m/->CatchNode class sym body attrs meta parent)))


(defn catch-with-class [node class] (assoc node :class class))
(defn catch-with-sym   [node sym]   (assoc node :sym sym))
(defn catch-with-body  [node body]  (assoc node :body body))

;; ══════════════════════════════════════════════
;; ThrowNode
;; ══════════════════════════════════════════════
(defn throw-expr [node] (:expr node))

(defn make-throw
  ([expr]                      (m/->ThrowNode expr {} nil nil))
  ([expr attrs]                (m/->ThrowNode expr attrs nil nil))
  ([expr attrs meta]           (m/->ThrowNode expr attrs meta nil))
  ([expr attrs meta parent]    (m/->ThrowNode expr attrs meta parent)))

(defn throw-with-expr [node expr] (assoc node :expr expr))

;; ══════════════════════════════════════════════
;; VectorNode
;; ══════════════════════════════════════════════
(defn vec-items    [node] (:items node))   ;; 别名
(defn vector-items [node] (:items node))

(defn make-vector
  ([items]                      (m/->VectorNode items {} nil nil))
  ([items attrs]                (m/->VectorNode items attrs nil nil))
  ([items attrs meta]           (m/->VectorNode items attrs meta nil))
  ([items attrs meta parent]    (m/->VectorNode items attrs meta parent)))

(defn vector-with-items [node items] (assoc node :items (vec items)))

;; ══════════════════════════════════════════════
;; MapNode
;; ══════════════════════════════════════════════
(defn map-kvs [node] (:kvs node))

(defn make-map
  ([kvs]                      (m/->MapNode kvs {} nil nil))
  ([kvs attrs]                (m/->MapNode kvs attrs nil nil))
  ([kvs attrs meta]           (m/->MapNode kvs attrs meta nil))
  ([kvs attrs meta parent]    (m/->MapNode kvs attrs meta parent)))

(defn map-with-kvs [node kvs] (assoc node :kvs (vec kvs)))

;; ══════════════════════════════════════════════
;; ConvertNode
;; ══════════════════════════════════════════════
(defn convert-expr   [node] (:expr node))
(defn convert-src-ty [node] (:src-ty node))
(defn convert-dst-ty [node] (:dst-ty node))
(defn convert-cost    [node] (:cost node))

(defn make-convert
  ([expr src-ty dst-ty cost]                      (m/->ConvertNode expr src-ty dst-ty cost {} nil nil))
  ([expr src-ty dst-ty cost attrs]                (m/->ConvertNode expr src-ty dst-ty cost attrs nil nil))
  ([expr src-ty dst-ty cost attrs meta]           (m/->ConvertNode expr src-ty dst-ty cost attrs meta nil))
  ([expr src-ty dst-ty cost attrs meta parent]    (m/->ConvertNode expr src-ty dst-ty cost attrs meta parent)))

(defn convert-with-expr [node expr] (assoc node :expr expr))

(defn convert-node? [node] (= (some-> node ir2p/kind) :convert))

;; ══════════════════════════════════════════════
;; NsNode
;; ══════════════════════════════════════════════
(defn namespace-name       [node] (:name node))
(defn namespace-docstring  [node] (:docstring node))
(defn namespace-attr-map   [node] (:attr-map node))
(defn namespace-references [node] (:references node))

(defn make-ns
  ([name references]                      (m/->NsNode name nil nil references {} nil nil))
  ([name docstring attr-map references]    (m/->NsNode name docstring attr-map references {} nil nil))
  ([name docstring attr-map references attrs] (m/->NsNode name docstring attr-map references attrs nil nil))
  ([name docstring attr-map references attrs meta] (m/->NsNode name docstring attr-map references attrs meta nil))
  ([name docstring attr-map references attrs meta parent] (m/->NsNode name docstring attr-map references attrs meta parent)))

(defn ns-with-name       [node name]       (assoc node :name name))
(defn ns-with-references [node references] (assoc node :references references))
(defn ns-node? [node] (= (some-> node ir2p/kind) :ns))
;; ══════════════════════════════════════════════
;; RecordNode
;; ══════════════════════════════════════════════
(defn record-name      [node] (:name node))
(defn record-fields    [node] (:fields node))
(defn record-protocols [node] (:protocols node))

(defn make-record
  ([name fields protocols]                      (m/->RecordNode name fields protocols {} nil nil))
  ([name fields protocols attrs]                (m/->RecordNode name fields protocols attrs nil nil))
  ([name fields protocols attrs meta]           (m/->RecordNode name fields protocols attrs meta nil))
  ([name fields protocols attrs meta parent]    (m/->RecordNode name fields protocols attrs meta parent)))

(defn record-with-name      [node name]      (assoc node :name name))
(defn record-with-fields    [node fields]    (assoc node :fields fields))
(defn record-with-protocols [node protocols] (assoc node :protocols protocols))
(defn record-node? [node] (= (some-> node ir2p/kind) :record))

;; ══════════════════════════════════════════════
;; ProtocolNode
;; ══════════════════════════════════════════════
;; 访问器
(defn protocol-name  [node] (:name node))
(defn protocol-funcs [node] (:funcs node))        ;; 原 protocol-method-sigs

;; 构造函数
(defn make-protocol
  ([name funcs]                      (m/->ProtocolNode name funcs {} nil nil))
  ([name funcs attrs]                (m/->ProtocolNode name funcs attrs nil nil))
  ([name funcs attrs meta]           (m/->ProtocolNode name funcs attrs meta nil))
  ([name funcs attrs meta parent]    (m/->ProtocolNode name funcs attrs meta parent)))

;; 更新器
(defn protocol-with-name  [node name]  (assoc node :name name))
(defn protocol-with-funcs [node funcs] (assoc node :funcs funcs))  ;; 原 protocol-with-method-sigs

;; ══════════════════════════════════════════════
;; MemberAccessNode
;; ══════════════════════════════════════════════
;; 访问器
(defn access-target [node] (:target node))
(defn access-member [node] (:accessor node))   ; 原 `name` 统一为 `accessor`
(defn access-args   [node] (:args node))

;; 向后兼容别名（如果有代码仍用旧名）
(defn member-access-target   [node] (access-target node))
(defn member-access-accessor [node] (access-member node))
(defn member-access-args     [node] (access-args node))

(defn keyword-access? [node]
  (keyword? (access-member node)))

;; 构造函数
(defn make-member-access
  ([target accessor args]
   (m/->MemberAccessNode target accessor args nil nil))
  ([target accessor args meta]
   (m/->MemberAccessNode target accessor args meta nil))
  ([target accessor args meta parent]
   (m/->MemberAccessNode target accessor args meta parent)))

;; 更新器
(defn member-access-with-target   [node target]   (assoc node :target target))
(defn member-access-with-accessor [node accessor] (assoc node :accessor accessor))
(defn member-access-with-args     [node args]     (assoc node :args args))


;; top.kzre.homunculus.core.ir2.node
(defn wrap-body
  "若 exprs 包含多个表达式，返回 BlockNode 包装；单个则直接返回该节点；空则 nil。"
  [exprs]
  (case (count exprs)
    0 nil
    1 (first exprs)
    (make-block (vec exprs))))

(defn unwrap-body
  "若节点是 :block，返回其内部的表达式向量；否则返回包含该节点的单元素向量。
   用于将可能被 DoNode/BlockNode 包裹的代码展平。"
  [node]
  (if (= (kind node) :block)
    (block-exprs node)
    [node]))

;; ══════════════════════════════════════════════
;; 字段描述（RecordNode 的 fields 条目）
;; ══════════════════════════════════════════════
(defn field-name
  "返回字段的名称符号。"
  [field]
  (:name field))

(defn field-meta
  "返回字段的元数据。"
  [field]
  (:meta field))

(defn field-init
  "返回字段的初始化表达式节点，可能为 nil。"
  [field]
  (:init field))

(defn field-with-init
  "用新的 init 节点替换字段的初始值，返回更新后的字段 map。"
  [field init-node]
  (assoc field :init init-node))

(defn make-field
  "创建一个字段描述 map。"
  [name init meta]
  {:name name :init init :meta meta})
