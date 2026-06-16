(ns top.kzre.homunculus.core.ir1.node
  "IR1 AST 节点字段的安全访问器、构造器与更新器。所有对节点内部关键字的直接操作都应通过此命名空间。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir1.model :as m]))


(defn binding-pairs [bindings]
  (partition 2 bindings))

;; ════════════════════════════════════════════════════════════
;; 通用节点访问
;; ════════════════════════════════════════════════════════════
(defn node-meta [node] (ir1p/node-meta node))
(defn kind     [node] (ir1p/kind node))
(defn children [node] (ir1p/children node))
(defn parent   [node] (:parent node))

(defn node-with-meta   [node meta]   (assoc node :meta meta))
(defn with-parent [node parent] (assoc node :parent parent))

;; ════════════════════════════════════════════════════════════
;; 字面量
;; ════════════════════════════════════════════════════════════
(defn lit-val [node] (:val node))

(defn make-literal
  ([val] (m/->LiteralNode val nil nil))
  ([val meta] (m/->LiteralNode val meta nil)))

(defn literal-with-val [node val] (assoc node :val val))

;; ════════════════════════════════════════════════════════════
;; 符号
;; ════════════════════════════════════════════════════════════
(defn sym-name [node] (:name node))

(defn make-symbol
  ([name] (m/->SymbolNode name nil nil))
  ([name meta] (m/->SymbolNode name meta nil))
  ([name meta parent] (m/->SymbolNode name meta parent)))

(defn symbol-with-name [node name] (assoc node :name name))

;; ════════════════════════════════════════════════════════════
;; 向量 / 映射
;; ════════════════════════════════════════════════════════════
(defn vec-items [node] (:items node))
(defn map-pairs [node] (:pairs node))

(defn make-vector
  ([items] (m/->VectorNode items nil nil))
  ([items meta] (m/->VectorNode items meta nil))
  ([items meta parent] (m/->VectorNode items meta parent)))

(defn make-map
  ([pairs] (m/->MapNode pairs nil nil))
  ([pairs meta] (m/->MapNode pairs meta nil))
  ([pairs meta parent] (m/->MapNode pairs meta parent)))

(defn vector-with-items [node items] (assoc node :items items))
(defn map-with-pairs    [node pairs] (assoc node :pairs pairs))

;; ════════════════════════════════════════════════════════════
;; 调用
;; ════════════════════════════════════════════════════════════
(defn call-op   [node] (:op node))
(defn call-args [node] (:args node))

(defn make-call
  ([op args] (m/->CallNode op args nil nil))
  ([op args meta] (m/->CallNode op args meta nil))
  ([op args meta parent] (m/->CallNode op args meta parent)))

(defn call-with-op   [node op]   (assoc node :op op))
(defn call-with-args [node args] (assoc node :args args))

;; ════════════════════════════════════════════════════════════
;; if
;; ════════════════════════════════════════════════════════════
(defn if-test [node] (:test node))
(defn if-then [node] (:then node))
(defn if-else [node] (:else node))

(defn make-if
  ([test then else] (m/->IfNode test then else nil nil))
  ([test then else meta] (m/->IfNode test then else meta nil))
  ([test then else meta parent] (m/->IfNode test then else meta parent)))

(defn if-with-test [node test] (assoc node :test test))
(defn if-with-then [node then] (assoc node :then then))
(defn if-with-else [node else] (assoc node :else else))

;; ════════════════════════════════════════════════════════════
;; do
;; ════════════════════════════════════════════════════════════
(defn do-exprs [node] (:exprs node))

(defn make-do
  ([exprs] (m/->DoNode exprs nil nil))
  ([exprs meta] (m/->DoNode exprs meta nil))
  ([exprs meta parent] (m/->DoNode exprs meta parent)))

(defn do-with-exprs [node exprs] (assoc node :exprs exprs))

;; ════════════════════════════════════════════════════════════
;; let
;; ════════════════════════════════════════════════════════════
(defn let-bindings [node] (:bindings node))
(defn let-body     [node] (:body node))

(defn make-let
  ([bindings body]
   (m/->LetNode bindings body (/ (count bindings) 2) nil nil))
  ([bindings body meta]
   (m/->LetNode bindings body (/ (count bindings) 2) meta nil))
  ([bindings body meta parent]
   (m/->LetNode bindings body (/ (count bindings) 2) meta parent)))

(defn make-binding
  "创建一个绑定对 [sym val]，其中 sym 和 val 为原始表单或 IR 节点。"
  [bind val]
  [bind val])

(defn let-with-bindings [node bindings] (assoc node :bindings bindings))
(defn let-with-body     [node body]      (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; fn
;; ════════════════════════════════════════════════════════════
(defn fn-name   [node] (:name node))
(defn fn-params [node] (:params node))
(defn fn-body   [node] (:body node))

(defn make-fn
  ([name params body] (m/->FnNode name params body nil nil))
  ([name params body meta] (m/->FnNode name params body meta nil)))

(defn fn-with-name   [node name]   (assoc node :name name))
(defn fn-with-params [node params] (assoc node :params params))
(defn fn-with-body   [node body]   (assoc node :body body))

;; ── 参数描述（param）─────────────────────
(defn param-sym  [param] (:name param))   ;; 注意：make-param 用的是 :name 键
(defn param-meta [param] (:meta param))

(defn param-with-sym  [param sym]  (assoc param :name sym))
(defn param-with-meta [param meta] (assoc param :meta meta))

;; ════════════════════════════════════════════════════════════
;; def
;; ════════════════════════════════════════════════════════════
(defn def-name [node] (:name node))
(defn def-doc  [node] (:doc node))
(defn def-attr [node] (:attr node))
(defn def-val  [node] (:val node))

(defn make-def
  ([name val] (m/->DefNode name nil nil val nil nil))
  ([name val meta] (m/->DefNode name nil nil val meta nil))
  ([name doc attr val meta] (m/->DefNode name doc attr val meta nil))
  ([name doc attr val meta parent] (m/->DefNode name doc attr val meta parent)))

(defn def-with-name [node name] (assoc node :name name))
(defn def-with-doc  [node doc]  (assoc node :doc doc))
(defn def-with-attr [node attr] (assoc node :attr attr))
(defn def-with-val  [node val]  (assoc node :val val))

;; ════════════════════════════════════════════════════════════
;; loop
;; ════════════════════════════════════════════════════════════
(defn loop-bindings [node] (:bindings node))
(defn loop-body     [node] (:body node))

(defn make-loop
  ([bindings body]
   (m/->LoopNode bindings body (/ (count bindings) 2) nil nil))
  ([bindings body meta]
   (m/->LoopNode bindings body (/ (count bindings) 2) meta nil))
  ([bindings body meta parent]
   (m/->LoopNode bindings body (/ (count bindings) 2) meta parent)))

(defn loop-with-bindings [node bindings] (assoc node :bindings bindings))
(defn loop-with-body     [node body]      (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; recur
;; ════════════════════════════════════════════════════════════
(defn recur-exprs [node] (:exprs node))

(defn make-recur
  ([exprs] (m/->RecurNode exprs nil nil))
  ([exprs meta] (m/->RecurNode exprs meta nil))
  ([exprs meta parent] (m/->RecurNode exprs meta parent)))

(defn recur-with-exprs [node exprs] (assoc node :exprs exprs))

;; ════════════════════════════════════════════════════════════
;; quote
;; ════════════════════════════════════════════════════════════
(defn quoted-expr [node] (:expr node))

(defn make-quote
  ([expr] (m/->QuoteNode expr nil nil))
  ([expr meta] (m/->QuoteNode expr meta nil))
  ([expr meta parent] (m/->QuoteNode expr meta parent)))

(defn quote-with-expr [node expr] (assoc node :expr expr))

;; ════════════════════════════════════════════════════════════
;; var
;; ════════════════════════════════════════════════════════════
(defn var-sym [node] (:var-sym node))

(defn make-var
  ([var-sym] (m/->VarNode var-sym nil nil))
  ([var-sym meta] (m/->VarNode var-sym meta nil))
  ([var-sym meta parent] (m/->VarNode var-sym meta parent)))

(defn var-with-sym [node var-sym] (assoc node :var-sym var-sym))

;; ════════════════════════════════════════════════════════════
;; throw
;; ════════════════════════════════════════════════════════════
(defn throw-expr [node] (:expr node))

(defn make-throw
  ([expr] (m/->ThrowNode expr nil nil))
  ([expr meta] (m/->ThrowNode expr meta nil))
  ([expr meta parent] (m/->ThrowNode expr meta parent)))

(defn throw-with-expr [node expr] (assoc node :expr expr))

;; ════════════════════════════════════════════════════════════
;; set!
;; ════════════════════════════════════════════════════════════
(defn set-var [node] (:var node))
(defn set-val [node] (:val node))

(defn make-set!
  ([var val] (m/->SetNode var val nil nil))
  ([var val meta] (m/->SetNode var val meta nil))
  ([var val meta parent] (m/->SetNode var val meta parent)))

(defn set-with-var [node var] (assoc node :var var))
(defn set-with-val [node val] (assoc node :val val))

;; ════════════════════════════════════════════════════════════
;; try / catch
;; ════════════════════════════════════════════════════════════
(defn try-body    [node] (:body node))
(defn try-catches [node] (:catches node))
(defn try-finally [node] (:finally node))

(defn make-try
  ([body catches] (m/->TryNode body catches nil nil nil))
  ([body catches finally] (m/->TryNode body catches finally nil nil))
  ([body catches finally meta] (m/->TryNode body catches finally meta nil))
  ([body catches finally meta parent] (m/->TryNode body catches finally meta parent)))

(defn try-with-body    [node body]    (assoc node :body body))
(defn try-with-catches [node catches] (assoc node :catches catches))
(defn try-with-finally [node finally] (assoc node :finally finally))

(defn catch-class [node] (:class node))
(defn catch-sym   [node] (:sym node))
(defn catch-body  [node] (:body node))

(defn make-catch
  ([class sym body] (m/->CatchNode class sym body nil nil))
  ([class sym body meta] (m/->CatchNode class sym body meta nil))
  ([class sym body meta parent] (m/->CatchNode class sym body meta parent)))

(defn catch-with-class [node class] (assoc node :class class))
(defn catch-with-sym   [node sym]   (assoc node :sym sym))
(defn catch-with-body  [node body]  (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; ns
;; ════════════════════════════════════════════════════════════
(defn namespace-name       [node] (:name node))
(defn namespace-references [node] (:references node))
(defn namespace-docstring  [node] (:docstring node))
(defn namespace-attr-map   [node] (:attr-map node))

(defn make-ns
  ([name references]
   (m/->NsNode name nil nil references nil nil))
  ([name docstring attr-map references meta]
   (m/->NsNode name docstring attr-map references meta nil))
  ([name docstring attr-map references meta parent]
   (m/->NsNode name docstring attr-map references meta parent)))

(defn ns-with-name       [node name]       (assoc node :name name))
(defn ns-with-references [node references] (assoc node :references references))

;; ════════════════════════════════════════════════════════════
;; record
;; ════════════════════════════════════════════════════════════
(defn record-name      [node] (:name node))
(defn record-fields    [node] (:fields node))
(defn record-protocols [node] (:protocols node))

(defn field-name [field] (:name field))
(defn field-meta [field] (:meta field))
(defn field-init [field] (:init field))

(defn make-field
  [name init meta]
  {:name name :init init :meta meta})

(defn field-with-init [field init] (assoc field :init init))
(defn field-with-meta [field meta] (assoc field :meta meta))

(defn make-param [name meta] {:name name :meta meta})

(defn make-arity
  "创建一个方法元数描述 map"
  [method-name params body meta]
  {:name method-name :params params :body body :meta meta})

(defn make-record
  ([name fields protocols] (m/->RecordNode name fields protocols nil nil))
  ([name fields protocols meta] (m/->RecordNode name fields protocols meta nil)))

(defn record-with-name      [node name]      (assoc node :name name))
(defn record-with-fields    [node fields]    (assoc node :fields fields))
(defn record-with-protocols [node protocols] (assoc node :protocols protocols))

;; ── 协议实现 (protocol-impl) ──────────────
;; 协议实现条目不是 IR 节点，而是普通 map，但为了一致性仍提供构造/更新函数。

(defn make-protocol-impl
  "创建一个协议实现条目，包含 :protocol 和 :methods。
   protocol 为符号，methods 为方法元数向量。"
  [protocol methods]
  {:protocol protocol :methods methods})

(defn protocol-impl-methods
  "读取协议实现的方法列表。"
  [impl]
  (:methods impl))

(defn protocol-impl-protocol
  "读取协议实现的协议名。"
  [impl]
  (:protocol impl))

(defn protocol-impl-with-methods
  "替换协议实现的方法列表。"
  [impl methods]
  (assoc impl :methods methods))

(defn protocol-impl-add-methods
  "向协议实现追加方法。"
  [impl more-methods]
  (update impl :methods into more-methods))

;; ── 协议方法签名（无 body，有返回类型）────
(defn make-protocol-method
  "创建一个协议方法签名。name 为符号，params 为参数描述向量，ret 为返回类型（如 :void），meta 为元数据。"
  [name params ret meta]
  {:name name :params params :ret ret :meta meta})

(defn protocol-method-name   [m] (:name m))
(defn protocol-method-params [m] (:params m))
(defn protocol-method-ret    [m] (:ret m))
(defn protocol-method-meta   [m] (:meta m))

(defn protocol-method-with-name   [m name]   (assoc m :name name))
(defn protocol-method-with-params [m params] (assoc m :params params))
(defn protocol-method-with-ret    [m ret]    (assoc m :ret ret))
(defn protocol-method-with-meta   [m meta]   (assoc m :meta meta))

;; ── 方法元数 (arity) ─────────────────────
(defn arity-name   [arity] (:name arity))
(defn arity-params [arity] (:params arity))
(defn arity-body   [arity] (:body arity))
(defn arity-meta   [arity] (:meta arity))

(defn arity-with-name   [arity name]   (assoc arity :name name))
(defn arity-with-params [arity params] (assoc arity :params params))
(defn arity-with-body   [arity body]   (assoc arity :body body))
(defn arity-with-meta   [arity meta]   (assoc arity :meta meta))

;; ── 协议实现 (protocol-impl) ──────────────
;; （保留原有 make-protocol-impl, protocol-impl-protocol, protocol-impl-methods 等）

(defn protocol-impl-map-methods
  "对协议实现中的每个方法应用 f，返回新的协议实现。"
  [impl f]
  (protocol-impl-with-methods impl (mapv f (protocol-impl-methods impl))))

;; ════════════════════════════════════════════════════════════
;; protocol
;; ════════════════════════════════════════════════════════════
(defn protocol-name  [node] (:name node))
(defn protocol-funcs [node] (:funcs node))

(defn make-protocol
  ([name funcs] (m/->ProtocolNode name funcs nil nil))
  ([name funcs meta] (m/->ProtocolNode name funcs meta nil)))

(defn protocol-with-name  [node name]  (assoc node :name name))
(defn protocol-with-funcs [node funcs] (assoc node :funcs funcs))

;; ════════════════════════════════════════════════════════════
;; member-access
;; ════════════════════════════════════════════════════════════
(defn member-access-target   [node] (:target node))
(defn member-access-accessor [node] (:accessor node))
(defn member-access-args     [node] (:args node))

(defn keyword-access? [node] (keyword? (:accessor node)))

;; 向后兼容别名
(defn access-target [node] (member-access-target node))
(defn access-member [node] (member-access-accessor node))
(defn access-args   [node] (member-access-args node))

(defn make-member-access
  ([target accessor args] (m/->MemberAccessNode target accessor args nil nil))
  ([target accessor args meta] (m/->MemberAccessNode target accessor args meta nil))
  ([target accessor args meta parent] (m/->MemberAccessNode target accessor args meta parent)))

(defn member-access-with-target   [node target]   (assoc node :target target))
(defn member-access-with-accessor [node accessor] (assoc node :accessor accessor))
(defn member-access-with-args     [node args]     (assoc node :args args))




(defn wrap-body
  "若 exprs 包含多个表达式，则包装为 DoNode；若单个表达式，直接返回；若空则返回 nil。"
  [exprs]
  (case (count exprs)
    0 nil
    1 (first exprs)
    (make-do (vec exprs))))