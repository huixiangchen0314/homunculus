(ns top.kzre.homunculus.backend.shader.lower
  "IR2 → ShaderAST 降级器。统一按表达式降级，依赖死代码消除清理冗余。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.backend.shader.metadata :as md]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.irstmt.ast :as irstmt]
    [top.kzre.homunculus.core.types.type :as ty]))

(def ^:private unary-ops #{'! '- '++ '--})
(def ^:private infix-ops #{'+ '- '* '/ '% '== '!= '< '> '<= '>= '&& '||})

(defrecord Env [locals])
(defn make-env [] (->Env #{}))
(defn env-add-local [env var-name] (update env :locals conj var-name))
(defn env-contains? [env var-name] (contains? (:locals env) var-name))
(defonce empty-env (make-env))

(defn- ir-meta [node] (irstmt/node-meta node))
(declare lower-ast)

;; 辅助：将子节点列表按表达式降级
(defn- lower-expr-list [env nodes]
  (reduce (fn [[exprs e] node]
            (let [[expr ne] (lower-ast node e)]
              [(conj exprs expr) ne]))
          [[] env] nodes))


;; 多方法
(defmulti lower-ast (fn [node _env] (irstmt/kind node)))

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
                 ;; 结构体构造函数 TODO 使用符号表查询
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
        target-ty (ty/get-type target)
        len (when (ty/vec-type? target-ty)
              (ty/value-val (ty/vec-size target-ty)))]
    (if (integer? len)
      [(ast/->Literal len (ir-meta node)) env']
      (throw (ex-info "Cannot determine array length at compile time" {:node node})))))

(defmethod lower-ast :record [node env]
  (let [struct-name (n/record-name node)
        fields (n/record-fields node)
        members (mapv (fn [f] (ast/->StructMember (:name f) (ty/get-type f) (irstmt/node-meta f))) fields)]
    [(ast/->Struct struct-name members (ir-meta node)) env]))

;; 复合结构
(defmethod lower-ast :block [node env]
  (let [stmts (:stmts node)
        ret   (:ret node)
        [lowered-stmts env'] (reduce (fn [[s e] stmt]
                                       (let [[stmt' e'] (lower-ast stmt e)]
                                         [(conj s stmt') e']))
                                     [[] env] stmts)
        [ret' env''] (if ret (lower-ast ret env') [nil env'])]
    [(ast/->Block lowered-stmts ret' (irstmt/node-meta node)) env'']))

(defmethod lower-ast :if [node env]
  (let [[test env1] (lower-ast (:test node) env)
        [then-block env2] (lower-ast (:then node) env1)
        [else-block env3] (if-let [e (:else node)]
                            (lower-ast e env2)
                            [nil env2])]
    [(ast/->If test then-block else-block (irstmt/node-meta node)) env3]))

(defmethod lower-ast :assign [node env]
  (let [lhs-node (n/assign-var node)
        lhs-type (ty/get-type lhs-node)
        [lhs env1] (lower-ast lhs-node env)
        [rhs env2] (lower-ast (n/assign-val node) env1)]
    (if (and (ty/vec-type? lhs-type)
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

(defmethod lower-ast :var-decl [node env]
  (let [var-name (:name node)
        init-expr (:val node)
        [init env'] (if init-expr (lower-ast init-expr env) [nil env])
        resource-kind (md/shader-resource-kind node)]
    (if resource-kind
      (let [slot (case resource-kind
                   :texture2D (md/shader-texture-register node)
                   :sampler   (md/shader-sampler-register node)
                   :cbuffer   (md/shader-cbuffer-register node)
                   nil)
            members (when (= resource-kind :cbuffer)
                      (mapv (fn [[sym type-sym]]
                              (ast/->StructMember sym (ty/make-tcon type-sym) nil))
                            (md/shader-cbuffer-members node)))]
        [(ast/->ResourceDecl var-name resource-kind slot members (irstmt/node-meta node)) env'])
      (let [var-ty (ty/get-type node)
            uniform? (md/shader-uniform? node)
            static-var? (md/shader-static-var? node)]
        (cond
          uniform?    [(ast/->Uniform  var-name var-ty (irstmt/node-meta node)) env']
          static-var? [(ast/->StaticVar var-name var-ty init (irstmt/node-meta node)) env']
          :else       (if (env-contains? env' var-name)
                        [(ast/->Assign (ast/->Variable var-name nil) init (irstmt/node-meta node)) env']
                        [(ast/->VarDecl var-name var-ty init (irstmt/node-meta node)) (env-add-local env' var-name)]))))))

(defmethod lower-ast :function [node env]
  (let [name (:name node)
        params (:params node)
        body (:body node)
        [body-node env'] (lower-ast body env)
        ret-ty (ty/fun-return-type (ty/get-type node))
        stage (md/shader-stage node)
        param-nodes (mapv (fn [p] (ast/->Param (:name p) (ty/get-type p) (irstmt/node-meta p))) params)]
    (if stage
      [(ast/->EntryPoint name stage ret-ty param-nodes body-node (irstmt/node-meta node)) env']
      [(ast/->Function   name       ret-ty param-nodes body-node (irstmt/node-meta node)) env'])))

(defmethod lower-ast :default [node _env]
  (throw (ex-info (str "Lowering not implemented for IRStmt node: " (irstmt/kind node)) {:node node})))


;; 顶层入口：收集所有语句，丢弃顶层的 ret
(defn lower-nodes [nodes]
  (let [[stmts _] (reduce (fn [[stmts env] n]
                            (let [[node new-env] (lower-ast n env)]
                              [(conj stmts node) new-env]))
                          [[] empty-env] nodes)]
    stmts))