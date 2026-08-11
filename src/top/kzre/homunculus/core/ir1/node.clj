(ns top.kzre.homunculus.core.ir1.node
  "IR1 AST 节点字段的安全访问器、构造器与更新器。所有对节点内部关键字的直接操作都应通过此命名空间。"
  (:require
    [top.kzre.homunculus.core.ir1.ast :as m]))

(defn protocol-name [node] (:name node))          ; 返回符号
(defn protocol-methods [node] (:methods node))    ; IR1 Method 向量
(defn method-name [m] (:name m))
(defn method-params [m] (:params m))              ; IR1 Param 向量
(defn method-docstring [m] (:docstring m))

;; ════════════════════════════════════════════════════════════
;; 通用节点访问
;; ════════════════════════════════════════════════════════════
(defn node-meta [node] (m/node-meta node))

;; ════════════════════════════════════════════════════════════
;; 字面量
;; ════════════════════════════════════════════════════════════
(defn lit-val [node] (:val node))

(defn make-literal
  ([val] (m/->Literal val nil))
  ([val meta] (m/->Literal val meta)))

;; ════════════════════════════════════════════════════════════
(defn sym-name [node] (:name node))

(defn make-symbol
  ([name] (m/->Symbol name nil))
  ([name meta] (m/->Symbol name meta)))

(defn vec-items [node] (:items node))
(defn map-pairs [node] (:pairs node))

(defn make-vector
  ([items] (m/->Vector items nil))
  ([items meta] (m/->Vector items meta)))

(defn make-map
  ([pairs] (m/->Map pairs nil))
  ([pairs meta] (m/->Map pairs meta)))

(defn vector-with-items [node items] (assoc node :items items))
(defn map-with-pairs    [node pairs] (assoc node :pairs pairs))

;; ════════════════════════════════════════════════════════════
;; 调用
;; ════════════════════════════════════════════════════════════
(defn call-op   [node] (:op node))
(defn call-args [node] (:args node))

(defn make-call
  ([op args] (m/->Call op args nil))
  ([op args meta] (m/->Call op args meta)))

(defn call-with-op   [node op]   (assoc node :op op))
(defn call-with-args [node args] (assoc node :args args))

;; ════════════════════════════════════════════════════════════
;; if
;; ════════════════════════════════════════════════════════════
(defn if-test [node] (:test node))
(defn if-then [node] (:then node))
(defn if-else [node] (:else node))

(defn make-if
  ([test then else] (m/->If test then else nil ))
  ([test then else meta] (m/->If test then else meta )))

(defn if-with-test [node test] (assoc node :test test))
(defn if-with-then [node then] (assoc node :then then))
(defn if-with-else [node else] (assoc node :else else))

;; ════════════════════════════════════════════════════════════
;; do
;; ════════════════════════════════════════════════════════════
(defn do-exprs [node] (:exprs node))

(defn make-do
  ([exprs] (m/->Do exprs nil))
  ([exprs meta] (m/->Do exprs meta)))

(defn do-with-exprs [node exprs] (assoc node :exprs exprs))

;; ════════════════════════════════════════════════════════════
;; let
;; ════════════════════════════════════════════════════════════
(defn let-bindings [node]
  (:bindings node))
(defn let-body     [node] (:body node))

(defn make-let
  ([bindings body]
   (m/->Let bindings body nil))
  ([bindings body meta]
   (m/->Let bindings body meta)))


(defn let-with-bindings [node bindings] (assoc node :bindings bindings))
(defn let-with-body     [node body]      (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; fn
;; ════════════════════════════════════════════════════════════
(defn fn-name   [node] (:name node))
(defn fn-params [node] (:params node))
(defn fn-body   [node] (:body node))

(defn make-fn
  ([name params body] (m/->Fn name params body nil ))
  ([name params body meta] (m/->Fn name params body meta )))

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
(defn def-docstring  [node] (:docstring node))
(defn def-attr [node] (:attr node))
(defn def-val  [node] (:val node))

(defn make-def
  ([name val] (m/->Def name val nil nil))
  ([name val meta] (m/->Def name val nil  meta ))
  ([name val docstring meta] (m/->Def name  val docstring meta)))

(defn def-with-name [node name] (assoc node :name name))
(defn def-with-docstring  [node docstring]  (assoc node :docstring docstring))
(defn def-with-attr [node attr] (assoc node :attr attr))
(defn def-with-val  [node val]  (assoc node :val val))

;; ════════════════════════════════════════════════════════════
;; loop
;; ════════════════════════════════════════════════════════════
(defn loop-bindings [node] (:bindings node))
(defn loop-body     [node] (:body node))

(defn make-loop
  ([bindings body]
   (m/->Loop bindings body nil))
  ([bindings body meta]
   (m/->Loop bindings body meta)))

(defn loop-with-bindings [node bindings] (assoc node :bindings bindings))
(defn loop-with-body     [node body]      (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; recur
;; ════════════════════════════════════════════════════════════
(defn recur-exprs [node] (:exprs node))

(defn make-recur
  ([exprs] (m/->Recur exprs nil))
  ([exprs meta] (m/->Recur exprs meta)))

(defn recur-with-exprs [node exprs] (assoc node :exprs exprs))

;; ════════════════════════════════════════════════════════════
;; quote
;; ════════════════════════════════════════════════════════════
(defn quoted-expr [node] (:expr node))

(defn make-quote
  ([expr] (m/->Quote expr nil))
  ([expr meta] (m/->Quote expr meta)))

(defn quote-with-expr [node expr] (assoc node :expr expr))

;; ════════════════════════════════════════════════════════════
;; var
;; ════════════════════════════════════════════════════════════
(defn var-sym [node] (:var-sym node))

(defn make-var
  ([var-sym] (m/->Var var-sym nil))
  ([var-sym meta] (m/->Var var-sym meta)))

(defn var-with-sym [node var-sym] (assoc node :var-sym var-sym))

;; ════════════════════════════════════════════════════════════
;; throw
;; ════════════════════════════════════════════════════════════
(defn throw-expr [node] (:expr node))

(defn make-throw
  ([expr] (m/->Throw expr nil))
  ([expr meta] (m/->Throw expr meta)))

(defn throw-with-expr [node expr] (assoc node :expr expr))

;; ════════════════════════════════════════════════════════════
;; set!
;; ════════════════════════════════════════════════════════════
(defn set-var [node] (:var node))
(defn set-val [node] (:val node))

(defn make-set!
  ([var val] (m/->Set var val nil))
  ([var val meta] (m/->Set var val meta)))

(defn set-with-var [node var] (assoc node :var var))
(defn set-with-val [node val] (assoc node :val val))

;; ════════════════════════════════════════════════════════════
;; try / catch
;; ════════════════════════════════════════════════════════════
(defn try-body    [node] (:body node))
(defn try-catches [node] (:catches node))
(defn try-finally [node] (:finally node))

(defn make-try
  ([body catches] (m/->Try body catches nil nil))
  ([body catches finally] (m/->Try body catches finally nil))
  ([body catches finally meta] (m/->Try body catches finally meta)))

(defn try-with-body    [node body]    (assoc node :body body))
(defn try-with-catches [node catches] (assoc node :catches catches))
(defn try-with-finally [node finally] (assoc node :finally finally))

(defn catch-class [node] (:class node))
(defn catch-sym   [node] (:sym node))
(defn catch-body  [node] (:body node))

(defn make-catch
  ([class sym body] (m/->Catch class sym body nil ))
  ([class sym body meta] (m/->Catch class sym body meta )))

(defn catch-with-class [node class] (assoc node :class class))
(defn catch-with-sym   [node sym]   (assoc node :sym sym))
(defn catch-with-body  [node body]  (assoc node :body body))

;; ════════════════════════════════════════════════════════════
;; ns
;; ════════════════════════════════════════════════════════════
(defn namespace-name       [node] (:name node))
(defn namespace-requires [node] (:requires node))
(defn namespace-docstring  [node] (:docstring node))
(defn make-ns
  ([name requires]
   (m/->Ns name requires nil nil))
  ([name requires docstring meta]
   (m/->Ns name requires docstring meta)))

(defn ns-with-name       [node name]       (assoc node :name name))
(defn ns-with-references [node references] (assoc node :requires references))

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

(defn make-param [name meta] (m/->Param name meta))

(defn make-arity
  "创建一个方法元数描述 map"
  [method-name params body meta]
  {:name method-name :params params :body body :meta meta})

(defn make-record
  ([name fields protocols] (m/->Record name fields protocols nil))
  ([name fields protocols meta] (m/->Record name fields protocols meta)))

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



(defn protocol-impl-map-methods
  "对协议实现中的每个方法应用 f，返回新的协议实现。"
  [impl f]
  (protocol-impl-with-methods impl (mapv f (protocol-impl-methods impl))))

;; ════════════════════════════════════════════════════════════
;; protocol
;; ════════════════════════════════════════════════════════════

(defn make-protocol
  ([name funcs] (m/->Protocol name funcs nil))
  ([name funcs meta] (m/->Protocol name funcs meta)))


(defn member-access-target   [node] (:target node))
(defn member-access-accessor [node] (:accessor node))
(defn member-access-args     [node] (:args node))

;; 向后兼容别名
(defn access-target [node] (member-access-target node))
(defn access-member [node] (member-access-accessor node))
(defn access-args   [node] (member-access-args node))

(defn make-member-access
  ([target accessor args] (m/->MemberAccess target accessor args nil))
  ([target accessor args meta] (m/->MemberAccess target accessor args meta)))


(defn wrap-body
  "若 exprs 包含多个表达式，则包装为 DoNode；若单个表达式，直接返回；若空则返回 nil。"
  ([exprs]
   (wrap-body exprs nil))
  ([exprs meta]
   (case (count exprs)
     0 nil
     (m/->Do (vec exprs) meta))))