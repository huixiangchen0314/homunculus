(ns top.kzre.homunculus.backend.shader.lower
  "IR2 → ShaderAST 降级器。统一按表达式降级，依赖死代码消除清理冗余。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.backend.shader.metadata :as md]))

(def ^:private unary-ops #{'! '- '++ '--})
(def ^:private infix-ops #{'+ '- '* '/ '% '== '!= '< '> '<= '>= '&& '||})

(defrecord Env [locals])
(defn make-env [] (->Env #{}))
(defn env-add-local [env var-name] (update env :locals conj var-name))
(defn env-contains? [env var-name] (contains? (:locals env) var-name))
(defonce empty-env (make-env))

(defn- ir-meta [node] (n/node-meta node))
(declare lower-ast)

;; 辅助：将子节点列表按表达式降级
(defn- lower-expr-list [env nodes]
  (reduce (fn [[exprs e] node]
            (let [[expr ne] (lower-ast node e)]
              [(conj exprs expr) ne]))
          [[] env] nodes))


;; 多方法
(defmulti lower-ast (fn [node _env] (ir2/kind node)))

;; 基础节点
(defmethod lower-ast :ns [node env]
  (let [requires (n/namespace-requires node)
        deps (mapv first requires)
        imports (mapv #(ast/->Import % (ir-meta node)) deps)]
    [(ast/->Block imports nil (ir-meta node)) env]))

(defmethod lower-ast :literal [node env]
  [(ast/->Literal (n/lit-val node) (ir-meta node)) env])

(defmethod lower-ast :variable [node env]
  [(ast/->Variable (:name node) (ir-meta node)) env])

(defmethod lower-ast :call [node env]
  (let [op-sym (:name (n/call-fn node))
        [args env'] (lower-expr-list env (n/call-args node))
        result (cond
                 (and (contains? unary-ops op-sym) (= (count args) 1))
                 (ast/->UnaryOp op-sym (first args) (ir-meta node))
                 (and (contains? infix-ops op-sym) (= (count args) 2))
                 (ast/->BinaryOp op-sym (first args) (second args) (ir-meta node))
                 ;; 结构体构造函数
                 (str/starts-with? (name op-sym) "->")
                 (ast/->Constructor (ty/get-type node) args
                                    (assoc (ir-meta node) :struct? true))
                 :else
                 (ast/->Call op-sym args (ir-meta node)))]
    [result env']))

(defmethod lower-ast :vector [node env]
  (let [[items env'] (lower-expr-list env (n/vector-items node))
        vec-ty (ty/get-type node)]
    [(ast/->Constructor vec-ty items (ir-meta node)) env']))

(defmethod lower-ast :member-access [node env]
  (let [[target env'] (lower-ast (n/access-target node) env)
        member (n/access-member node)
        args (n/access-args node)]
    (if (empty? args)
      [(ast/->MemberAccess target member (ir-meta node)) env']
      (throw (ex-info "Method call not yet supported" {:node node})))))

(defmethod lower-ast :convert [node env]
  (let [[expr env'] (lower-ast (n/convert-expr node) env)
        dst-ty (n/convert-dst-ty node)]
    [(ast/->Cast dst-ty expr (ir-meta node)) env']))

(defmethod lower-ast :new-array [node env]
  [(ast/->Literal nil (ir-meta node)) env])

(defmethod lower-ast :aget [node env]
  (let [[target env1] (lower-ast (n/aget-target node) env)
        [idx env2] (lower-ast (n/aget-idx node) env1)]
    [(ast/->ArrayIndex target idx (ir-meta node)) env2]))

(defmethod lower-ast :aset [node env]
  (let [[target env1] (lower-ast (n/aset-target node) env)
        [idx env2] (lower-ast (n/aset-idx node) env1)
        [val env3] (lower-ast (n/aset-val node) env2)]
    [(ast/->Assign (ast/->ArrayIndex target idx (ir-meta node)) val (ir-meta node)) env3]))

(defmethod lower-ast :alength [node env]
  (let [[_ env'] (lower-ast (n/alength-target node) env)
        target (n/alength-target node)
        target-ty (ty/get-type target)]
    (if-let [len (when (ty/vec-type? target-ty) (ty/vec-size target-ty))]
      [(ast/->Literal len (ir-meta node)) env']
      (throw (ex-info "Cannot determine array length at compile time" {:node node})))))

(defmethod lower-ast :record [node env]
  (let [struct-name (n/record-name node)
        fields (n/record-fields node)
        members (mapv (fn [f] (ast/->StructMember (:name f) (ty/get-type f) nil)) fields)]
    [(ast/->Struct struct-name members (ir-meta node)) env]))

;; 复合结构
(defmethod lower-ast :block [node env]
  (let [children (n/block-exprs node)
        [stmts env1] (reduce (fn [[stmts e] child]
                               (let [[stmt ne] (lower-ast child e)]
                                 [(conj stmts stmt) ne]))
                             [[] env] (butlast children))
        [last-expr env2] (lower-ast (last children) env1)]
    [(ast/->Block stmts last-expr (ir-meta node)) env2]))

(defmethod lower-ast :if [node env]
  (let [tmp-name (gensym "ifval")
        tmp-type (ty/get-type node)
        tmp-var (ast/->Variable tmp-name nil)
        [test env1] (lower-ast (n/if-test node) env)
        [then-val env2] (lower-ast (n/if-then node) env1)
        [else-val env3] (if-let [e (n/if-else node)]
                          (lower-ast e env2)
                          [nil env2])
        then-assign (ast/->Assign tmp-var then-val nil)
        then-block (ast/->Block [then-assign] nil (ir-meta (n/if-then node)))
        else-assign (ast/->Assign tmp-var else-val nil)
        else-block (ast/->Block [else-assign] nil (ir-meta (n/if-else node)))
        if-stmt (ast/->If test then-block else-block (ir-meta node))
        tmp-decl (ast/->VarDecl tmp-name tmp-type nil (ir-meta node))]
    [(ast/->Block [tmp-decl if-stmt] tmp-var (ir-meta node)) env3]))

(defmethod lower-ast :let [node env]
  (let [bindings (n/let-bindings node)
        [decls env1] (reduce (fn [[stmts e] b]
                               (let [var-node (:var b)
                                     val-node (:val b)
                                     var-ty (ty/get-type var-node)
                                     var-name (:name var-node)
                                     [init-expr e2] (lower-ast val-node e)
                                     ;; 判断是否为需要标记的数组初始化（非构造函数且非nil）
                                     vec-init? (and (some? init-expr)
                                                    (ty/vec-type? var-ty) (pos? (ty/vec-size var-ty))
                                                    (not= :constructor (ast/kind init-expr))
                                                    (not= :literal (ast/kind init-expr)))
                                     stmt
                                     (if vec-init?
                                       ;; 生成 Block：包含无初始化 VarDecl + 标记的 Assign，ret 为变量本身
                                       (let [decl (ast/->VarDecl var-name var-ty nil (ir-meta var-node))
                                             assign (ast/->Assign (ast/->Variable var-name nil) init-expr
                                                                  (assoc (ir-meta var-node)
                                                                    :vec-assign? true
                                                                    :vec-type var-ty))]
                                         (ast/->Block [decl assign] (ast/->Variable var-name nil) nil))
                                       ;; 普通变量处理
                                       (if (env-contains? e2 var-name)
                                         (ast/->Assign (ast/->Variable var-name nil) init-expr (ir-meta var-node))
                                         (ast/->VarDecl var-name var-ty init-expr (ir-meta var-node))))
                                     e3 (if vec-init?
                                          (env-add-local e2 var-name)
                                          (if (env-contains? e2 var-name)
                                            e2
                                            (env-add-local e2 var-name)))]
                                 [(conj stmts stmt) e3]))
                             [[] env] bindings)
        [body-expr env'] (lower-ast (n/let-body node) env1)]
    [(ast/->Block decls body-expr (ir-meta node)) env']))

(defmethod lower-ast :assign [node env]
  (let [lhs-node (n/assign-var node)
        lhs-type (ty/get-type lhs-node)
        [lhs env1] (lower-ast lhs-node env)
        [rhs env2] (lower-ast (n/assign-val node) env1)]
    (if (and (ty/vec-type? lhs-type)
             (pos? (ty/vec-size lhs-type))
             (not= :literal (ast/kind rhs))
             (not= :constructor (ast/kind rhs)))
      [(ast/->Assign lhs rhs (assoc (ir-meta node)
                               :vec-assign? true
                               :vec-type lhs-type)) env2]
      [(ast/->Assign lhs rhs (ir-meta node)) env2])))

(defmethod lower-ast :while [node env]
  (let [[test env1] (lower-ast (n/while-test node) env)
        [body env2] (lower-ast (n/while-body node) env1)
        body-block (if (= :block (ast/kind body)) body (ast/->Block [body] nil (ir-meta (n/while-body node))))
        ]
    [(ast/->Block [(ast/->While test body-block (ir-meta node))] nil (ir-meta node)) env2]))


(defmethod lower-ast :define [node env]
  (let [meta (n/node-meta node)]
    (if-let [res-kind (:shader/resource-kind meta)]
      ;; 资源声明
      (let [res-name (n/define-name node)
            slot (case res-kind
                   :texture2D (:shader/texture-register meta)
                   :sampler   (:shader/sampler-register meta)
                   :cbuffer   (:shader/cbuffer-register meta)
                   nil)
            members (when (= res-kind :cbuffer)
                      (mapv (fn [[sym type-sym]]
                              (ast/->StructMember sym (ty/make-tcon type-sym) nil))
                            (:shader/cbuffer-members meta)))]
        [(ast/->ResourceDecl res-name res-kind slot members (ir-meta node)) env])
      (let [val (n/define-val node)]
        (if (and val (= :lambda (ir2/kind val)))
          ;; 函数/入口点
          (let [lam   val
                stage (md/shader-stage node)
                ret-ty (ty/fun-return-type (ty/get-type lam))
                params (n/lambda-params lam)
                param-nodes (mapv (fn [p] (ast/->Param (:name p) (ty/get-type p) nil)) params)
                [body-node env'] (lower-ast (n/lambda-body lam) env)
                body-block (if (= :block (ast/kind body-node))
                             body-node
                             (ast/->Block [] body-node (ir-meta node)))]
            (if stage
              [(ast/->EntryPoint (n/define-name node) stage ret-ty param-nodes body-block (ir-meta node)) env']
              [(ast/->Function   (n/define-name node)       ret-ty param-nodes body-block (ir-meta node)) env']))
          ;; 变量声明（顶层）
          (let [var-ty (ty/get-type node)
                var-name (n/define-name node)
                [init-expr env'] (if val (lower-ast val env) [nil env])
                uniform?    (:shader/uniform? meta)
                static-var? (:shader/static-var? meta)
                ;; 标记条件：非构造器且非nil的数组初始化
                vec-init? (and (some? init-expr)
                             (ty/vec-type? var-ty)
                             (pos? (ty/vec-size var-ty))
                             (not= :constructor (ast/kind init-expr))
                             (not= :literal (ast/kind init-expr)))]
            (cond
              uniform?    [(ast/->Uniform  var-name var-ty (ir-meta node)) env']
              static-var? [(ast/->StaticVar var-name var-ty init-expr (ir-meta node)) env']
              vec-init?
              ;; 生成无初始化的声明和带标记的赋值语句块
              (let [decl (ast/->VarDecl var-name var-ty nil (ir-meta node))
                    assign (ast/->Assign (ast/->Variable var-name nil) init-expr
                                         (assoc meta :vec-assign? true :vec-type var-ty))]
                [(ast/->Block [decl assign] (ast/->Variable var-name nil) (ir-meta node)) env'])
              :else
              [(ast/->VarDecl var-name var-ty init-expr (ir-meta node)) env'])))))))

(defmethod lower-ast :default [node _env]
  (throw (ex-info (str "Lowering not implemented for " (ir2/kind node)) {:node node})))

;; 顶层入口：收集所有语句，丢弃顶层的 ret
(defn lower-nodes [nodes]
  (let [[stmts _] (reduce (fn [[stmts env] n]
                            (let [[node new-env] (lower-ast n env)]
                              [(conj stmts node) new-env]))
                          [[] empty-env] nodes)]
    stmts))