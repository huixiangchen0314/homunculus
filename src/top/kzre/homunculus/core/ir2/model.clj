(ns top.kzre.homunculus.core.ir2.model
  "IR2 语言无关 AST 的节点记录定义。所有节点实现 INode 协议。
   使用 reduce-children，每个节点直接处理字段。")

(defprotocol INode
  (kind       [this])
  (children   [this])
  (reduce-children [this f env])
  (attrs      [this])
  (node-meta  [this]))


;; ── Param 节点（补充 INode 实现） ──
(defrecord Param [name attrs meta]
  INode
  (kind [_] :param)
  (children [_] [])                   ; 叶子节点，无子节点
  (reduce-children [this _f env]      ; 不遍历子节点，直接返回自身
    [this env])
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── Lambda 节点（保持原有逻辑，兼容 Param 向量） ──
(defrecord Lambda [params body captures fn-name attrs meta]
  INode
  (kind [_] :lambda)
  (children [_] (into (vec params) [body]))   ; params 是 Param 节点向量
  (reduce-children [this f env]
    (let [[new-params env1] (reduce (fn [[ps e] p]   ; p 是 Param 节点
                                      (let [[np e2] (f p e)]
                                        [(conj ps np) e2]))
                                    [[] env] params)
          [new-body env2] (f body env1)]
      [(assoc this :params new-params :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── LiteralNode ─────────────────────────────
(defrecord Literal [val attrs meta ]
  INode
  (kind [_] :literal)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── VariableNode ────────────────────────────
(defrecord Variable [name attrs meta]
  INode
  (kind [_] :variable)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── CallNode ────────────────────────────────
(defrecord Call [fn args attrs meta ]
  INode
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
(defrecord If [test then else attrs meta ]
  INode
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
(defrecord Block [exprs attrs meta ]
  INode
  (kind [_] :block)
  (children [_] (vec exprs))
  (reduce-children [this f env]
    (let [[new-exprs env1] (reduce (fn [[es e] expr]
                                     (let [[ne e2] (f expr e)] [(conj es ne) e2])) [[] env] exprs)]
      [(assoc this :exprs new-exprs) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── LetNode ─────────────────────────────────
(defrecord Binding [var val attrs meta]
  INode
  (kind [_] :binding)
  (children [_] [var val])
  (reduce-children [this f env]
    (let [[new-var env1] (f var env)
          [new-val env2] (f val env1)]
      [(assoc this :var new-var :val new-val) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

(defrecord Let [bindings body attrs meta]
  INode
  (kind [_] :let)
  (children [_]
    (conj (vec bindings) body))
  (reduce-children [this f env]
    (let [[new-bindings env1]
          (reduce (fn [[bnds e] binding]
                    (let [[new-binding e2] (f binding e)]
                      [(conj bnds new-binding) e2]))
                  [[] env] bindings)
          [new-body env2] (f body env1)]
      [(assoc this :bindings new-bindings :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── LoopNode ────────────────────────────────
(defrecord Loop [bindings body attrs meta]
  INode
  (kind [_] :loop)
  (children [_] (conj (vec bindings) body))
  (reduce-children [this f env]
    (let [[new-bindings env1]
          (reduce (fn [[bnds e] binding]
                    (let [[new-binding e2] (f binding e)]
                      [(conj bnds new-binding) e2]))
                  [[] env] bindings)
          [new-body env2] (f body env1)]
      [(assoc this :bindings new-bindings :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── RecurNode ───────────────────────────────
(defrecord Recur [args attrs meta ]
  INode
  (kind [_] :recur)
  (children [_] (vec args))
  (reduce-children [this f env]
    (let [[new-args env1] (reduce (fn [[as e] a] (let [[na e2] (f a e)] [(conj as na) e2])) [[] env] args)]
      [(assoc this :args new-args) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── DefineNode ──────────────────────────────
(defrecord Define [name val docstring attrs meta ]
  INode
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
(defrecord Vector [items attrs meta ]
  INode
  (kind [_] :vector)
  (children [_] (vec items))
  (reduce-children [this f env]
    (let [[new-items env1] (reduce (fn [[is e] item] (let [[ni e2] (f item e)] [(conj is ni) e2])) [[] env] items)]
      [(assoc this :items new-items) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── MapNode ─────────────────────────────────

(defrecord Pair [key val attrs meta]
  INode
  (kind [_] :pair)
  (children [_] [key val])
  (reduce-children [this f env]
    (let [[new-key env1] (f key env)
          [new-val env2] (f val env1)]
      [(assoc this :key new-key :val new-val) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

(defrecord Map [pairs attrs meta ]
  INode
  (kind [_] :map)
  (children [_] (vec pairs))
  (reduce-children [this f env]
    (let [[new-pairs env1] (reduce (fn [[ks e] pair] (let [[nk e2] (f pair e)] [(conj ks nk) e2])) [[] env] pairs)]
      [(assoc this :pairs new-pairs) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── TryNode ─────────────────────────────────
(defrecord Try [body catches finally attrs meta]
  INode
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
(defrecord Catch [class sym body attrs meta ]
  INode
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
(defrecord Throw [expr attrs meta ]
  INode
  (kind [_] :throw)
  (children [_] [expr])
  (reduce-children [this f env]
    (let [[new-expr env1] (f expr env)]
      [(assoc this :expr new-expr) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── AssignNode ──────────────────────────────
(defrecord Assign [var val attrs meta ]
  INode
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
(defrecord While [test body attrs meta ]
  INode
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
(defrecord Convert [expr src-ty dst-ty cost attrs meta ]
  INode
  (kind [_] :convert)
  (children [_] [expr])
  (reduce-children [this f env]
    (let [[new-expr env1] (f expr env)]
      [(assoc this :expr new-expr) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)

;; ── NsNode ──────────────────────────────────
(defrecord Ns [name requires docstring attrs meta ]
  INode
  (kind [_] :ns)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta)
)


;; ── MethodNode ──────────────────────────────
(defrecord Method [name params body docstring attrs meta]
  INode
  (kind [_] :method)
  ;; children：params 向量，若 body 非空则追加 body
  (children [_] (cond-> (vec params) body (conj body)))
  (reduce-children [this f env]
    ;; 先递归处理所有 params，再处理 body（若存在）
    (let [[new-params env1] (reduce (fn [[ps e] p]
                                      (let [[np e2] (f p e)]
                                        [(conj ps np) e2]))
                                    [[] env] params)
          [new-body env2] (if body
                            (f body env1)
                            [nil env1])]
      [(assoc this :params new-params :body new-body) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))



;; ── FieldNode─────────────────────────
(defrecord Field [name attrs meta]
  INode
  (kind [_] :field)
  (children [_] [])
  (reduce-children [this _f env] [this env])
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── ProtocolImplNode）───────────────────
(defrecord ProtocolImpl [proto-name methods attrs meta]
  INode
  (kind [_] :protocol-impl)
  (children [_] (vec methods))                  ; 子节点为 Method 向量
  (reduce-children [this f env]
    (let [[new-methods env1] (reduce (fn [[ms e] m]
                                       (let [[nm e2] (f m e)]
                                         [(conj ms nm) e2]))
                                     [[] env] methods)]
      [(assoc this :methods new-methods) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta))

(defrecord Record [name fields protocols attrs meta]
  INode
  (kind [_] :record)
  ;; 子节点：所有字段 + 所有协议实现（字段目前无子节点，但保留在列表中以便未来扩展）
  (children [_] (into (vec fields) protocols))
  (reduce-children [this f env]
    ;; 顺序遍历 fields（无子节点，直接保留）和 protocols，传递环境
    (let [[new-fields env1] (reduce (fn [[fs e] field]
                                      (let [[new-field e2] (f field e)]
                                        [(conj fs new-field) e2]))
                                    [[] env] fields)
          [new-protocols env2] (reduce (fn [[ps e] proto-impl]
                                         (let [[new-proto e2] (f proto-impl e)]
                                           [(conj ps new-proto) e2]))
                                       [[] env1] protocols)]
      [(assoc this :fields new-fields :protocols new-protocols) env2]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── ProtocolNode ────────────────────────────
(defrecord Protocol [name methods attrs meta]
  INode
  (kind [_] :protocol)
  (children [_] (vec methods))                  ;; 子节点为 Method 向量
  (reduce-children [this f env]
    (let [[new-methods env1] (reduce (fn [[ms e] m]
                                       (let [[nm e2] (f m e)]
                                         [(conj ms nm) e2]))
                                     [[] env] methods)]
      [(assoc this :methods new-methods) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── MemberAccessNode ────────────────────────
(defrecord MemberAccess [target accessor args attrs meta ]
  INode
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
(defrecord NewArray [size attrs meta]
  INode
  (kind [_] :new-array)
  (children [_] (if (satisfies? INode size) [size] []))
  (reduce-children [this f env]
    (if (satisfies? INode size)
      (let [[new-size env'] (f size env)]
        [(assoc this :size new-size) env'])
      [this env]))
  (attrs [_] attrs)
  (node-meta [_] meta))

;; ── AGetNode ────────────────────────────────
(defrecord AGet [target idx attrs meta ]
  INode
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
(defrecord ASet [target idx val attrs meta ]
  INode
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
(defrecord ALength [target attrs meta ]
  INode
  (kind [_] :alength)
  (children [_] [target])
  (reduce-children [this f env]
    (let [[new-target env1] (f target env)]
      [(assoc this :target new-target) env1]))
  (attrs [_] attrs)
  (node-meta [_] meta)
)