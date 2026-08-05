(ns top.kzre.homunculus.backend.shader.lower
  "IR2 → ShaderAST 降级器。忠实翻译 Lisp 语义，Block 的 ret 必须在此阶段确定。"
  (:require
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

(defmulti lower-ast (fn [node _env] (ir2/kind node)))

(defmethod lower-ast :ns [node env]
  (let [requires (n/namespace-requires node)
        deps (mapv first requires)
        imports (mapv #(ast/->Import % (ir-meta node)) deps)]
    [(ast/->Block imports nil (ir-meta node)) env]))

(defmethod lower-ast :literal [node env]
  [(ast/->Literal (n/lit-val node) (ir-meta node)) env])

(defmethod lower-ast :variable [node env]
  [(ast/->Variable (:name node) (ir-meta node)) env])

(defmethod lower-ast :block [node env]
  (let [children (n/block-exprs node)
        [stmts final-env] (reduce (fn [[stmts e] child]
                                    (let [[stmt ne] (lower-ast child e)]
                                      [(conj stmts stmt) ne]))
                                  [[] env] (butlast children))
        last-child (last children)
        [last-node env2] (lower-ast last-child final-env)]
    ;; 直接将最后一个节点作为 ret，不进行任何展开或合并
    [(ast/->Block stmts last-node (ir-meta node)) env2]))

(defmethod lower-ast :if [node env]
  ;; if 作为表达式：生成临时变量，创建 if 语句进行赋值，返回 Block 的 ret 为临时变量
  (let [tmp-name (gensym "ifval")
        tmp-type (ty/get-type node)
        tmp-var (ast/->Variable tmp-name nil)
        [test-expr env1] (lower-ast (n/if-test node) env)
        [then-val env2] (lower-ast (n/if-then node) env1)
        [else-val env3] (if-let [e (n/if-else node)]
                          (lower-ast e env2)
                          [nil env2])
        then-assign (ast/->Assign tmp-var then-val nil)
        then-block (ast/->Block [then-assign] nil (ir-meta (n/if-then node)))
        else-assign (ast/->Assign tmp-var else-val nil)
        else-block (ast/->Block [else-assign] nil (ir-meta (n/if-else node)))
        if-stmt (ast/->If test-expr then-block else-block (ir-meta node))
        tmp-decl (ast/->VarDecl tmp-name tmp-type nil (ir-meta node))]
    [(ast/->Block [tmp-decl if-stmt] tmp-var (ir-meta node)) env3]))

(defmethod lower-ast :call [node env]
  (let [op-sym (:name (n/call-fn node))
        [args final-env] (reduce (fn [[args e] arg]
                                   (let [[arg-node ne] (lower-ast arg e)]
                                     [(conj args arg-node) ne]))
                                 [[] env] (n/call-args node))
        result (cond
                 (and (contains? unary-ops op-sym) (= (count args) 1))
                 (ast/->UnaryOp op-sym (first args) (ir-meta node))
                 (and (contains? infix-ops op-sym) (= (count args) 2))
                 (ast/->BinaryOp op-sym (first args) (second args) (ir-meta node))
                 :else
                 (ast/->Call op-sym args (ir-meta node)))]
    [result final-env]))

(defmethod lower-ast :vector [node env]
  (let [[items final-env] (reduce (fn [[items e] item]
                                    (let [[item-node ne] (lower-ast item e)]
                                      [(conj items item-node) ne]))
                                  [[] env] (n/vector-items node))
        elem-ty (ty/vec-element-type (ty/get-type node))]
    [(ast/->Constructor elem-ty items (ir-meta node)) final-env]))

(defmethod lower-ast :let [node env]
  (let [bindings (n/let-bindings node)
        [decls-stmts final-env]
        (reduce (fn [[stmts e] b]
                  (let [var-node (:var b)
                        val-node (:val b)
                        var-ty (ty/get-type var-node)
                        var-name (:name var-node)
                        [init-expr env1] (lower-ast val-node e)   ;; init-expr 已经是单个节点，不会是 Block（除非值本身就是 if 表达式，那也是 Block，正确）
                        [stmt new-env]
                        (if (env-contains? e var-name)
                          [(ast/->Assign (ast/->Variable var-name nil) init-expr (ir-meta var-node))
                           env1]
                          [(ast/->VarDecl var-name var-ty init-expr (ir-meta var-node))
                           (env-add-local env1 var-name)])]
                    [(conj stmts stmt) new-env]))
                [[] env] bindings)
        [body-node _] (lower-ast (n/let-body node) final-env)]
    [(ast/->Block decls-stmts body-node (ir-meta node)) final-env]))

(defmethod lower-ast :assign [node env]
  (let [[lhs env1] (lower-ast (n/assign-var node) env)
        [rhs env2] (lower-ast (n/assign-val node) env1)]
    [(ast/->Assign lhs rhs (ir-meta node)) env2]))

(defmethod lower-ast :while [node env]
  (let [[test env1] (lower-ast (n/while-test node) env)
        [body env2] (lower-ast (n/while-body node) env1)
        body-block (if (= :block (ast/kind body)) body (ast/->Block [body] nil (ir-meta (n/while-body node))))]
    [(ast/->While test body-block (ir-meta node)) env2]))

(defmethod lower-ast :member-access [node env]
  (let [[target env1] (lower-ast (n/access-target node) env)
        member (n/access-member node)
        args (n/access-args node)]
    (if (empty? args)
      [(ast/->MemberAccess target member (ir-meta node)) env1]
      (throw (ex-info "Method call not yet supported" {:node node})))))

(defmethod lower-ast :convert [node env]
  (let [[expr env1] (lower-ast (n/convert-expr node) env)
        dst-ty (n/convert-dst-ty node)]
    [(ast/->Cast dst-ty expr (ir-meta node)) env1]))

(defmethod lower-ast :new-array [node env]
  (let [[size env1] (lower-ast (n/new-array-size node) env)]
    [(ast/->Constructor (ty/get-type node) [size] (ir-meta node)) env1]))

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
  (let [target (n/alength-target node)
        target-ty (ty/get-type target)]
    (if-let [len (when (ty/vec-type? target-ty) (ty/vec-size target-ty))]
      [(ast/->Literal len (ir-meta node)) env]
      (throw (ex-info "Cannot determine array length at compile time" {:node node})))))

(defmethod lower-ast :define [node env]
  (let [meta (n/node-meta node)]
    (if-let [res-kind (:shader/resource-kind meta)]
      ;; 资源声明保持不变
      (let [res-name (n/define-name node)
            slot     (case res-kind
                       :texture2D (:shader/texture-register meta)
                       :sampler   (:shader/sampler-register meta)
                       :cbuffer   (:shader/cbuffer-register meta)
                       nil)
            members  (when (= res-kind :cbuffer)
                       (mapv (fn [[sym type-sym]]
                               (ast/->StructMember sym (ty/make-tcon type-sym) nil))
                             (:shader/cbuffer-members meta)))]
        [(ast/->ResourceDecl res-name res-kind slot members (ir-meta node)) env])
      (let [val (n/define-val node)]
        (if (and val (= :lambda (ir2/kind val)))
          ;; 函数/入口点：body 保留完整 Block，不再提取 ret
          (let [lam   val
                stage (md/shader-stage node)
                ret-ty (ty/fun-return-type (ty/get-type lam))
                params (n/lambda-params lam)
                param-nodes (mapv (fn [p] (ast/->Param (:name p) (ty/get-type p) nil)) params)
                [body-node _] (lower-ast (n/lambda-body lam) env)]
            (if stage
              [(ast/->EntryPoint (n/define-name node) stage ret-ty param-nodes body-node (ir-meta node)) env]
              [(ast/->Function   (n/define-name node)       ret-ty param-nodes body-node (ir-meta node)) env]))
          ;; 变量声明
          (let [init (when val (first (lower-ast val env)))]
            [(ast/->VarDecl (n/define-name node) (ty/get-type node) init (ir-meta node)) env]))))))

(defmethod lower-ast :record [node env]
  (let [struct-name (n/record-name node)   ;; 直接是符号
        fields (n/record-fields node)
        members (mapv (fn [f] (ast/->StructMember (:name f) (ty/get-type f) nil)) fields)]
    [(ast/->Struct struct-name members (ir-meta node)) env]))

(defmethod lower-ast :default [node _env]
  (throw (ex-info (str "Lowering not implemented for " (ir2/kind node)) {:node node})))

(defn lower-nodes [nodes]
  (let [[stmts _] (reduce (fn [[stmts env] n]
                            (let [[stmt new-env] (lower-ast n env)]
                              [(conj stmts stmt) new-env]))
                          [[] empty-env] nodes)]
    stmts))