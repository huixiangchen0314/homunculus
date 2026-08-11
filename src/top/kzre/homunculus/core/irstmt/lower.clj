(ns top.kzre.homunculus.core.irstmt.lower
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defrecord Env [locals])
(defn make-env [] (->Env #{}))
(defn env-add-local [env var-name] (update env :locals conj var-name))
(defn env-contains? [env var-name] (contains? (:locals env) var-name))
(defonce empty-env (make-env))

(defmulti lower-ast (fn [node _env] (ir2/kind node)))

(defn walk [node env] (ir2/reduce-children node lower-ast env))

(defmethod lower-ast :default
 [node env]
 (let [[node' new-env] (walk node env)
       node-kind (ir2/kind node')
       new-node
       (case node-kind
         ;; 表达式
         :literal     (ast/map->Literal node')
         :variable    (ast/map->Variable node')
         :call        (ast/map->Call node')
         :if          (ast/map->If node')
         :while       (ast/map->While node')
         :assign      (ast/map->Assign node')
         :convert     (ast/map->Convert node')
         :member-access (ast/map->MemberAccess node')
         :vector      (ast/map->Vector node')
         :map         (ast/map->Map node')
         :new-array   (ast/map->NewArray node')
         :aget        (ast/map->Aget node')
         :aset        (ast/map->Aset node')
         :alength     (ast/map->Alength node')
         :throw       (ast/map->Throw node')
         ;; 顶层/类型节点
         :ns          (ast/map->Ns node')
         :record      (ast/map->Record node')
         :protocol    (ast/map->Protocol node')
         :try         (ast/map->Try node')
         :catch       (ast/map->Catch node')
         ;; 其他
         :define      (ast/map->VarDecl node')
         :param       (ast/map->Param node')
         :pair        (ast/map->Pair node')
         :field       (ast/map->Field node')
         :method      (ast/map->Method node')
         :protocol-impl (ast/map->ProtocolImpl node')
         (throw (ex-info (str "Unsupported IR2 kind in IRStmt lower: " (ir2/kind node')) {:node node'})))]
   [new-node new-env]))

(defmethod lower-ast :block [node env]
  (let [children (n/block-exprs node)
        [stmts env1] (reduce (fn [[stmts e] child]
                               (let [[stmt ne] (lower-ast child e)]
                                 [(conj stmts stmt) ne]))
                             [[] env] (butlast children))
        [last-expr env2] (lower-ast (last children) env1)]
    [(ast/->Block stmts last-expr (n/attrs node) (n/node-meta node)) env2]))


(defmethod lower-ast :if [node env]
  (let [tmp-name (gensym "ifval")
        tmp-decl (ast/->VarDecl tmp-name nil
                                "Variable for if-expr"
                                (n/attrs node) (n/node-meta node))
        tmp-var (ast/->Variable tmp-name (n/attrs node) (n/node-meta node))
        [test env1] (lower-ast (n/if-test node) env)
        [then-val _] (lower-ast (n/if-then node) env1)
        [else-val _] (if-let [e (n/if-else node)]
                          (lower-ast e env1)
                          [nil env1])
        then-assign (ast/->Assign tmp-var then-val nil nil)
        then-node (n/if-then node)
        then-block (ast/->Block [then-assign] nil (n/attrs then-node) (n/node-meta then-node))
        else-assign (ast/->Assign tmp-var else-val nil nil)
        else-node (n/if-else node)
        else-block (ast/->Block [else-assign] nil (n/attrs else-node) (n/node-meta else-node))
        if-stmt (ast/->If test then-block else-block (n/attrs node) (n/node-meta node))
        ]
    [(ast/->Block [tmp-decl if-stmt] tmp-var (n/attrs node) (n/node-meta node)) env1]))

(defmethod lower-ast :let [node env]
  (let [bindings (n/let-bindings node)
        [decls env1] (reduce (fn [[stmts e] b]
                               (let [[init-expr e1] (lower-ast (:val b) e)
                                     [var-node e2] (lower-ast (:var b) e1)
                                     var-name (:name var-node)]
                                 (if (env-contains? e2 var-name)
                                   [(conj stmts
                                          (ast/->Assign var-node init-expr (ast/attrs var-node) (ast/node-meta var-node)))
                                    e2]
                                   [(conj stmts
                                          (ast/->VarDecl var-name init-expr
                                                         "Variable generated from let binding"
                                                         (ast/attrs var-node) (ast/node-meta var-node)))
                                    (env-add-local e2 var-name)])))
                             [[] env] bindings)
        [body-expr _] (lower-ast (n/let-body node) env1)]
    [(ast/->Block decls body-expr (n/attrs node) (n/node-meta node)) env1]))

(defmethod lower-ast :define [node env]
  (let [val (n/define-val node)]
    (if (and val (= :lambda (ir2/kind val)))
      ;; 函数/入口点
      (let [lam   val
            params (n/lambda-params lam)
            param-nodes (mapv (fn [p] (ast/->Param (:name p) (n/attrs p) (n/node-meta p))) params)
            [body-node env'] (lower-ast (n/lambda-body lam) env)]
        [(ast/->Function (n/define-name node) param-nodes body-node (n/attrs node) (n/node-meta node)) env'])
      ;; 变量声明
      (let [var-name (n/define-name node)
            [init-expr env'] (if val (lower-ast val env) [nil env])]
        [(ast/->VarDecl var-name init-expr (n/define-docstring node) (n/attrs node) (n/node-meta node)) env']))))

(defmethod lower-ast :while [node env]
  (let [[test env1] (lower-ast (n/while-test node) env)
        [body _] (lower-ast (n/while-body node) env1)
        ]
    [(ast/->While test body (n/attrs node) (n/node-meta node)) env1]))

(defn lower-nodes [nodes]
  (let [[stmts _] (reduce (fn [[stmts env] n]
                            (let [[node new-env] (lower-ast n env)]
                              [(conj stmts node) new-env]))
                          [[] empty-env] nodes)]
    stmts))