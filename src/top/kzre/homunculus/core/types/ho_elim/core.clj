(ns top.kzre.homunculus.core.types.ho-elim.core
  "基于类型信息的高阶函数内联 Pass。
   遍历 IR2 树，在 context 内部的 :env 中收集定义；
   当调用点的实参类型包含函数类型时，利用环境中的定义进行内联。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.subst.api :as subst]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmulti eliminate
          (fn [node depth context] (n/kind node)))

;; 辅助函数
(defn- env [context] (get context :env {}))
(defn- add-def [context name def-node]
  (update context :env assoc name def-node))

;; 叶子节点：返回自身，上下文不变
(defmethod eliminate :literal   [node _ ctx] [node ctx])
(defmethod eliminate :variable  [node _ ctx] [node ctx])

;; 定义：将自身加入环境，然后处理值
(defmethod eliminate :define [node depth ctx]
  (let [name (n/define-name node)
        ctx' (add-def ctx name node)]
    (if-let [val (n/define-val node)]
      (let [[new-val ctx''] (eliminate val (inc depth) ctx')]
        [(n/make-define name new-val (n/define-doc node)
                        (n/attrs node) (n/node-meta node) (n/parent node))
         ctx''])
      [node ctx'])))

;; 调用节点：尝试内联
(defmethod eliminate :call [node depth ctx]
  (let [max-depth (get ctx :ho-max-depth 20)
        fn-node (n/call-fn node)
        fn-name (when (n/variable-node? fn-node) (n/var-name fn-node))
        ;; 递归处理实参，收集上下文
        [args' ctx-after-args]
        (reduce (fn [[processed ctx] arg]
                  (let [[p c] (eliminate arg (inc depth) ctx)]
                    [(conj processed p) c]))
                [[] ctx]
                (n/call-args node))
        arg-tys (mapv ty/get-type args')
        has-fn-arg? (some ty/fun-type? arg-tys)
        current-env (env ctx-after-args)]
    (if (and fn-name (contains? current-env fn-name) has-fn-arg? (< depth max-depth))
      ;; 执行内联
      (let [def-node (get current-env fn-name)
            lam      (n/define-val def-node)
            params   (n/lambda-params lam)
            param-names (mapv n/var-name params)
            body     (n/lambda-body lam)
            inlined  (reduce (fn [b [pname arg]]
                               (subst/replace-var b pname arg))
                             body
                             (map vector param-names args'))]
        (eliminate inlined (inc depth) ctx-after-args))
      ;; 普通调用：递归处理函数部分
      (let [[new-fn ctx''] (eliminate fn-node (inc depth) ctx-after-args)]
        [(n/make-call new-fn args' (n/attrs node) (n/node-meta node) (n/parent node))
         ctx'']))))

;; 容器节点：递归处理子节点，传递上下文
(defmethod eliminate :if [node depth ctx]
  (let [[test ctx1] (eliminate (n/if-test node) depth ctx)
        [then ctx2] (eliminate (n/if-then node) depth ctx1)
        [else ctx3] (if-let [e (n/if-else node)]
                      (eliminate e depth ctx2)
                      [nil ctx2])]
    [(n/make-if test then else (n/attrs node) (n/node-meta node) (n/parent node))
     ctx3]))

(defmethod eliminate :block [node depth ctx]
  (let [[stmts ctx'] (reduce (fn [[processed ctx] expr]
                               (let [[p c] (eliminate expr depth ctx)]
                                 [(conj processed p) c]))
                             [[] ctx]
                             (n/block-exprs node))]
    [(n/make-block stmts (n/attrs node) (n/node-meta node) (n/parent node)) ctx']))

(defmethod eliminate :let [node depth ctx]
  (let [[bindings ctx'] (reduce (fn [[processed ctx] [v e]]
                                  (let [[new-v ctx1] (eliminate v depth ctx)
                                        [new-e ctx2] (eliminate e depth ctx1)]
                                    [(conj processed [new-v new-e]) ctx2]))
                                [[] ctx]
                                (n/let-bindings node))
        [body ctx''] (eliminate (n/let-body node) depth ctx')]
    [(n/make-let bindings body (n/attrs node) (n/node-meta node) (n/parent node)) ctx'']))

(defmethod eliminate :loop [node depth ctx]
  (let [[bindings ctx'] (reduce (fn [[processed ctx] [v e]]
                                  (let [[new-v ctx1] (eliminate v depth ctx)
                                        [new-e ctx2] (eliminate e depth ctx1)]
                                    [(conj processed [new-v new-e]) ctx2]))
                                [[] ctx]
                                (n/loop-bindings node))
        [body ctx''] (eliminate (n/loop-body node) depth ctx')]
    [(n/make-loop bindings body (n/attrs node) (n/node-meta node) (n/parent node)) ctx'']))

(defmethod eliminate :lambda [node depth ctx]
  (let [[params ctx'] (reduce (fn [[processed ctx] p]
                                (let [[new-p c] (eliminate p depth ctx)]
                                  [(conj processed new-p) c]))
                              [[] ctx]
                              (n/lambda-params node))
        [body ctx''] (eliminate (n/lambda-body node) depth ctx')]
    [(n/make-lambda params body (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     ctx'']))

(defmethod eliminate :recur [node depth ctx]
  (let [[args ctx'] (reduce (fn [[processed ctx] arg]
                              (let [[p c] (eliminate arg depth ctx)]
                                [(conj processed p) c]))
                            [[] ctx]
                            (n/recur-args node))]
    [(n/make-recur args (n/attrs node) (n/node-meta node) (n/parent node)) ctx']))

(defmethod eliminate :while [node depth ctx]
  (let [[test ctx1] (eliminate (n/while-test node) depth ctx)
        [body ctx2] (eliminate (n/while-body node) depth ctx1)]
    [(n/make-while test body (n/attrs node) (n/node-meta node) (n/parent node)) ctx2]))

;; 数组特殊节点
(defmethod eliminate :new-array [node depth ctx]
  (let [[size ctx'] (eliminate (n/new-array-size node) depth ctx)]
    [(n/make-new-array size (n/node-meta node) (n/parent node)) ctx']))
(defmethod eliminate :aget [node depth ctx]
  (let [[target ctx1] (eliminate (n/aget-target node) depth ctx)
        [idx ctx2]    (eliminate (n/aget-idx node) depth ctx1)]
    [(n/make-aget target idx (n/node-meta node) (n/parent node)) ctx2]))
(defmethod eliminate :aset [node depth ctx]
  (let [[target ctx1] (eliminate (n/aset-target node) depth ctx)
        [idx ctx2]    (eliminate (n/aset-idx node) depth ctx1)
        [val ctx3]    (eliminate (n/aset-val node) depth ctx2)]
    [(n/make-aset target idx val (n/node-meta node) (n/parent node)) ctx3]))
(defmethod eliminate :alength [node depth ctx]
  (let [[target ctx'] (eliminate (n/alength-target node) depth ctx)]
    [(n/make-alength target (n/node-meta node) (n/parent node)) ctx']))

(defmethod eliminate :default [node _ ctx] [node ctx])

;; 全局入口：使用 reduce 顺序处理根节点，累积环境
(defn process [ir2-roots context]
  (let [init-ctx (update context :env #(or % {}))
        [new-roots _] (reduce (fn [[nodes ctx] root]
                                (let [[new-root ctx'] (eliminate root 0 ctx)]
                                  [(conj nodes new-root) ctx']))
                              [[] init-ctx]
                              ir2-roots)]
    new-roots))

(defn make-context
  "构造约束生成所需的上下文 map。
   compile-ctx : 编译上下文
   frontend    : 前端协议实例（必须实现 IFrontendInfo）
   backend     : 后端协议实例（可选，用于类型转换）"
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:env {}
     :frontend frontend
     :ctx compile-ctx
     :backend backend
     :ho-max-depth 20
     :symbol-table symbols
     :known-types (sym/types-symbols symbols)}))