(ns top.kzre.homunculus.core.ir2.node
  "IR2 节点字段的安全访问器、构造器与更新器。所有对节点内部关键字的直接操作都应通过此命名空间。
   统一使用 make-* 构造函数，不再使用旧的 ->* 风格。"
  (:require [top.kzre.homunculus.core.ir2.ast :as ir2])
  (:import (top.kzre.homunculus.core.ir2.ast Binding)))


(defn make-field
  ([name]                      (ir2/->Field name {} nil))
  ([name attrs]                (ir2/->Field name attrs nil))
  ([name attrs meta]           (ir2/->Field name attrs meta)))

(defn make-protocol-impl
  ([proto-name methods]        (ir2/->ProtocolImpl proto-name methods {} nil))
  ([proto-name methods attrs]  (ir2/->ProtocolImpl proto-name methods attrs nil))
  ([proto-name methods attrs meta]
   (ir2/->ProtocolImpl proto-name methods attrs meta)))

(defn make-method
  ([name params docstring attrs meta]
   (ir2/->Method name params nil docstring attrs meta))
  ([name params body docstring attrs meta]
   (ir2/->Method name params body docstring attrs meta)))

(defn protocol-name [node] (:name node))          ; 返回符号
(defn protocol-methods [node] (:methods node))    ; IR1 Method 向量
(defn method-name [m] (:name m))
(defn method-params [m] (:params m))              ; IR1 Param 向量
(defn method-docstring [m] (:docstring m))


;; ══════════════════════════════════════════════
;; 通用字段访问
;; ══════════════════════════════════════════════
(defn kind [node]
  (when node
    (ir2/kind node)))

(defn attrs [node]
  (when node
    (ir2/attrs node)))

(defn node-meta [node]
  (when node
    (ir2/node-meta node)))

(defn children [node]
  (when node
    (ir2/children node)))

;; ── 类型操作 ──
(defn type-attr [node] (get-in node [:attrs :type]))
(defn set-type-attr [node ty] (assoc-in node [:attrs :type] ty))

;; ══════════════════════════════════════════════
;; VariableNode
;; ══════════════════════════════════════════════
(defn var-name [node] (:name node))

(defn make-variable
  ([name]                      (ir2/->Variable name {} nil))
  ([name attrs]                (ir2/->Variable name attrs nil))
  ([name attrs meta]           (ir2/->Variable name attrs meta)))

(defn variable-with-name [node name] (assoc node :name name))
(defn variable-with-attrs [node attrs] (assoc node :attrs attrs))

;; ══════════════════════════════════════════════
;; LiteralNode
;; ══════════════════════════════════════════════
(defn lit-val [node] (:val node))

(defn make-literal
  ([val]                      (ir2/->Literal val {} nil ))
  ([val attrs]                (ir2/->Literal val attrs nil ))
  ([val attrs meta]           (ir2/->Literal val attrs meta )))

(defn literal-with-val [node val] (assoc node :val val))

;; ══════════════════════════════════════════════
;; CallNode
;; ══════════════════════════════════════════════
(defn call-fn   [node] (:fn node))
(defn call-args [node] (:args node))

(defn make-call
  ([fn args]                      (ir2/->Call fn args {} nil ))
  ([fn args attrs]                (ir2/->Call fn args attrs nil ))
  ([fn args attrs meta]           (ir2/->Call fn args attrs meta )))

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
  ([test then else]                      (ir2/->If test then else {} nil ))
  ([test then else attrs]                (ir2/->If test then else attrs nil ))
  ([test then else attrs meta]           (ir2/->If test then else attrs meta )))

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
  ([exprs]                      (ir2/->Block exprs {} nil ))
  ([exprs attrs]                (ir2/->Block exprs attrs nil ))
  ([exprs attrs meta]           (ir2/->Block exprs attrs meta )))

(defn block-with-exprs [node exprs] (assoc node :exprs exprs))
(defn block-node? [bode] (= (kind bode) :block))

(defn let-bindings [node]
  (let [binds (:bindings node)]
    (if (and (seq binds) (instance? Binding (first binds)))
      binds
      (throw (ex-info "Invalid bindings form" {})))))

(defn let-body     [node] (:body node))

(defn make-let
  ([bindings body]                      (make-let bindings body {} nil))
  ([bindings body attrs]                (make-let bindings body attrs nil))
  ([bindings body attrs meta]
   (when (and (seq bindings) (not (instance? Binding (first bindings))))
     (throw (ex-info "make-let requires bindings to be Binding nodes, got old [var val] format"
                     {:bindings bindings})))
   (ir2/->Let (vec bindings) body attrs meta)))

(defn let-with-bindings [node bindings] (assoc node :bindings bindings))
(defn let-with-body     [node body]     (assoc node :body body))
(defn let-with-children [node bindings body]
  (-> node (assoc :bindings bindings) (assoc :body body)))

(defn make-param
  ([name]                      (ir2/->Param name {} nil))
  ([name attrs]                (ir2/->Param name attrs nil))
  ([name attrs meta]           (ir2/->Param name attrs meta)))

;; ══════════════════════════════════════════════
;; LambdaNode
;; ══════════════════════════════════════════════
(defn lambda-params   [node] (:params node))
(defn lambda-body     [node] (:body node))
(defn lambda-captures [node] (:captures node))
(defn lambda-fn-name  [node] (:fn-name node))

(defn make-lambda
  ([params body captures fn-name]                      (ir2/->Lambda params body captures fn-name {} nil ))
  ([params body captures fn-name attrs]                (ir2/->Lambda params body captures fn-name attrs nil ))
  ([params body captures fn-name attrs meta]           (ir2/->Lambda params body captures fn-name attrs meta )))

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
(defn define-docstring  [node] (:docstring node))

(defn make-define
  ([name val]                           (ir2/->Define name val nil {} nil ))
  ([name val docstring]                  (ir2/->Define name val docstring {} nil ))
  ([name val docstring attrs]            (ir2/->Define name val docstring attrs nil ))
  ([name val docstring attrs meta]       (ir2/->Define name val docstring attrs meta )))

(defn define-with-val [node val] (assoc node :val val))
(defn define-with-doc [node doc] (assoc node :docstring doc))

(defn define-node? [node] (= (some-> node ir2/kind) :define))

;; ══════════════════════════════════════════════
;; LoopNode
;; ══════════════════════════════════════════════
(defn loop-bindings [node]
  (:bindings node))

(defn loop-body     [node] (:body node))

(defn make-loop
  ([bindings body]                      (make-loop bindings body {} nil))
  ([bindings body attrs]                (make-loop bindings body attrs nil))
  ([bindings body attrs meta]
   (let [binds (if (and (seq bindings)
                        (not (instance? Binding (first bindings))))
                 ;; 旧格式 [var val] → Binding 向量，保留原有 attrs/meta 为空
                 (mapv (fn [[var val]] (ir2/->Binding var val {} nil)) bindings)
                 bindings)]
     (ir2/->Loop (vec binds) body attrs meta))))

(defn loop-with-bindings [node bindings] (assoc node :bindings bindings))
(defn loop-with-body     [node body]     (assoc node :body body))
(defn loop-with-children [node bindings body]
  (-> node (assoc :bindings bindings) (assoc :body body)))

;; ══════════════════════════════════════════════
;; RecurNode
;; ══════════════════════════════════════════════
(defn recur-args [node] (:args node))

(defn make-recur
  ([args]                      (ir2/->Recur args {} nil ))
  ([args attrs]                (ir2/->Recur args attrs nil ))
  ([args attrs meta]           (ir2/->Recur args attrs meta )))

(defn recur-with-args [node args] (assoc node :args args))

;; ══════════════════════════════════════════════
;; WhileNode
;; ══════════════════════════════════════════════
(defn while-test [node] (:test node))
(defn while-body [node] (:body node))

(defn make-while
  ([test body]                      (ir2/->While test body {} nil ))
  ([test body attrs]                (ir2/->While test body attrs nil ))
  ([test body attrs meta]           (ir2/->While test body attrs meta )))

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
  ([var val]                      (ir2/->Assign var val {} nil ))
  ([var val attrs]                (ir2/->Assign var val attrs nil ))
  ([var val attrs meta]           (ir2/->Assign var val attrs meta )))

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
   (ir2/->Try body catches finally {} nil ))
  ([body catches finally attrs]
   (ir2/->Try body catches finally attrs nil ))
  ([body catches finally attrs meta]
   (ir2/->Try body catches finally attrs meta )))

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
  ([class sym body]                      (ir2/->Catch class sym body {} nil ))
  ([class sym body attrs]                (ir2/->Catch class sym body attrs nil ))
  ([class sym body attrs meta]           (ir2/->Catch class sym body attrs meta )))


(defn catch-with-class [node class] (assoc node :class class))
(defn catch-with-sym   [node sym]   (assoc node :sym sym))
(defn catch-with-body  [node body]  (assoc node :body body))

;; ══════════════════════════════════════════════
;; ThrowNode
;; ══════════════════════════════════════════════
(defn throw-expr [node] (:expr node))

(defn make-throw
  ([expr]                      (ir2/->Throw expr {} nil ))
  ([expr attrs]                (ir2/->Throw expr attrs nil ))
  ([expr attrs meta]           (ir2/->Throw expr attrs meta )))

(defn throw-with-expr [node expr] (assoc node :expr expr))

;; ══════════════════════════════════════════════
;; VectorNode
;; ══════════════════════════════════════════════
(defn vec-items    [node] (:items node))   ;; 别名
(defn vector-items [node] (:items node))

(defn make-vector
  ([items]                      (ir2/->Vector items {} nil ))
  ([items attrs]                (ir2/->Vector items attrs nil ))
  ([items attrs meta]           (ir2/->Vector items attrs meta )))

(defn vector-with-items [node items] (assoc node :items (vec items)))

;; ══════════════════════════════════════════════
;; MapNode
;; ══════════════════════════════════════════════
(defn map-pairs [node] (:pairs node))

(defn make-map
  ([kvs]                      (ir2/->Map kvs {} nil ))
  ([kvs attrs]                (ir2/->Map kvs attrs nil ))
  ([kvs attrs meta]           (ir2/->Map kvs attrs meta )))

(defn map-with-kvs [node kvs] (assoc node :pairs (vec kvs)))

;; ══════════════════════════════════════════════
;; ConvertNode
;; ══════════════════════════════════════════════
(defn convert-expr   [node] (:expr node))
(defn convert-src-ty [node] (:src-ty node))
(defn convert-dst-ty [node] (:dst-ty node))
(defn convert-cost    [node] (:cost node))

(defn make-convert
  ([expr src-ty dst-ty ]                      (ir2/->Convert expr src-ty dst-ty {} nil ))
  ([expr src-ty dst-ty attrs]                (ir2/->Convert expr src-ty dst-ty attrs nil ))
  ([expr src-ty dst-ty attrs meta]           (ir2/->Convert expr src-ty dst-ty attrs meta )))

(defn convert-with-expr [node expr] (assoc node :expr expr))

(defn convert-node? [node] (= (some-> node ir2/kind) :convert))

;; ══════════════════════════════════════════════
;; NsNode
;; ══════════════════════════════════════════════
(defn namespace-name       [node] (:name node))
(defn namespace-docstring  [node] (:docstring node))

(defn namespace-requires [node] (:requires node))

(defn make-ns
  ([name requires]                      (ir2/->Ns name nil requires {} nil ))
  ([name requires  docstring]    (ir2/->Ns name requires docstring {} nil ))
  ([name requires  docstring attrs] (ir2/->Ns name requires docstring attrs nil ))
  ([name requires docstring attrs meta] (ir2/->Ns name requires docstring attrs meta)))

(defn ns-with-name       [node name]       (assoc node :name name))
(defn ns-with-references [node references] (assoc node :requires references))
(defn ns-node? [node] (= (some-> node ir2/kind) :ns))
;; ══════════════════════════════════════════════
;; RecordNode
;; ══════════════════════════════════════════════
(defn record-name      [node] (:name node))
(defn record-fields    [node] (:fields node))
(defn record-protocols [node] (:protocols node))

(defn make-record
  ([name fields protocols]                      (ir2/->Record name fields protocols {} nil ))
  ([name fields protocols attrs]                (ir2/->Record name fields protocols attrs nil ))
  ([name fields protocols attrs meta]           (ir2/->Record name fields protocols attrs meta )))

(defn record-with-name      [node name]      (assoc node :name name))
(defn record-with-fields    [node fields]    (assoc node :fields fields))
(defn record-with-protocols [node protocols] (assoc node :protocols protocols))
(defn record-node? [node] (= (some-> node ir2/kind) :record))

;; ══════════════════════════════════════════════
;; ProtocolNode
;; ══════════════════════════════════════════════
;; 访问器


;; 构造函数
(defn make-protocol
  ([name funcs]                      (ir2/->Protocol name funcs {} nil ))
  ([name funcs attrs]                (ir2/->Protocol name funcs attrs nil ))
  ([name funcs attrs meta]           (ir2/->Protocol name funcs attrs meta )))

;; 更新器
(defn protocol-with-name  [node name]  (assoc node :name name))

(defn protocol-with-funcs [node funcs] (assoc node :methods funcs))  ;; 原 protocol-with-method-sigs

;; ── ProtocolNode 方法访问器 ────────────────
(defn protocol-methods
  "返回协议节点的 funcs 向量，每一项为方法描述 map：
   {:name method-name, :params [...], :ret type, :meta ...}"
  [node]
  (:methods node))

(defn method-name
  "返回单个方法描述中的 :name。"
  [method-desc]
  (:name method-desc))

(defn method-params
  "返回单个方法描述中的 :params 向量，元素为形参描述 map：{:name sym, :meta ...}"
  [method-desc]
  (:params method-desc))

(defn method-ret
  "返回单个方法描述中的 :ret 类型。"
  [method-desc]
  (:ret method-desc))

(defn method-meta
  "返回单个方法描述中的 :meta。"
  [method-desc]
  (:meta method-desc))

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
;; top.kzre.homunculus.core.ir2.node
(defn make-member-access
  ([target accessor args]
   (ir2/->MemberAccess target accessor args {} nil ))
  ([target accessor args attrs]
   (ir2/->MemberAccess target accessor args attrs nil ))
  ([target accessor args attrs meta]
   (ir2/->MemberAccess target accessor args attrs meta )))

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



;; ── 节点类型判断（protocol-based，更安全） ──
(defn literal-node? [node] (= (kind node) :literal))
(defn variable-node? [node] (= (kind node) :variable))
(defn call-node? [node] (= (kind node) :call))
(defn if-node? [node] (= (kind node) :if))
(defn loop-node? [node] (= (kind node) :loop))
(defn vector-node? [node] (= (kind node) :vector))
(defn map-node? [node] (= (kind node) :map))
(defn member-access-node? [node] (= (kind node) :member-access))
(defn protocol-node? [node] (= (kind node) :protocol))


;; ── 数组特殊节点 ──────────────────────────
(defn new-array-node? [node] (= (kind node) :new-array))
(defn new-array-size [node] (:size node))

(defn make-new-array
  ([size]                      (ir2/->NewArray size {} nil ))
  ([size attrs]                (ir2/->NewArray size attrs nil ))
  ([size attrs meta]           (ir2/->NewArray size attrs meta )))

;; AGetNode
(defn aget-node? [node] (= (kind node) :aget))
(defn aget-target [node] (:target node))
(defn aget-idx    [node] (:idx node))

(defn make-aget
  ([target idx]              (ir2/->Aget target idx {} nil ))
  ([target idx attrs]        (ir2/->Aget target idx attrs nil ))
  ([target idx attrs meta]   (ir2/->Aget target idx attrs meta )))

;; ASetNode
(defn aset-node? [node] (= (kind node) :aset))
(defn aset-target [node] (:target node))
(defn aset-idx    [node] (:idx node))
(defn aset-val    [node] (:val node))

(defn make-aset
  ([target idx val]              (ir2/->Aset target idx val {} nil ))
  ([target idx val attrs]        (ir2/->Aset target idx val attrs nil ))
  ([target idx val attrs meta]   (ir2/->Aset target idx val attrs meta )))

;; ALengthNode
(defn alength-node? [node] (= (kind node) :alength))
(defn alength-target [node] (:target node))

(defn make-alength
  ([target]              (ir2/->Alength target {} nil ))
  ([target attrs]        (ir2/->Alength target attrs nil ))
  ([target attrs meta]   (ir2/->Alength target attrs meta )))

(defn binding-var [node]
  (:var node))

(defn binding-val [node]
  (:val node))



(defn diff?
  "判断两个节点是否不同（发生了折叠/变化）。忽略 attrs 和 meta。"
  [old-node new-node]
  (cond
    ;; 两个都是序列：比较长度，再递归比较每个位置
    (and (sequential? old-node) (sequential? new-node))
    (if (not= (count old-node) (count new-node))
      true
      (boolean (some (fn [[a b]] (diff? a b))
                     (map vector old-node new-node))))

    ;; 一个是序列另一个不是：发生了变化
    (or (sequential? old-node) (sequential? new-node))
    true

    ;; 两个都为 nil：无变化
    (and (nil? old-node) (nil? new-node))
    false

    ;; 一个为 nil：发生了变化
    (or (nil? old-node) (nil? new-node))
    true

    ;; 两个都是非 nil、非序列节点：比较 kind，再递归比较 children
    :else
    (if (not= (ir2/kind old-node) (ir2/kind new-node))
      true
      (diff? (ir2/children old-node) (ir2/children new-node)))))
