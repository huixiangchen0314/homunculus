(ns top.kzre.homunculus.core.types.ho-elim.core
  "高阶函数内联 Pass（仅处理命名高阶函数）。
   1. 分析阶段：遍历 IR 树，标记高阶函数定义（设置 attrs :ho?）。
   2. 内联阶段：顺序处理根节点，动态构建环境，内联可见的高阶调用。
      ho-max-depth 控制单次内联的最大展开深度，默认为 20。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.subst.replace :as replace]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.types.ho-elim.analyze :as analyze]
    [top.kzre.homunculus.internal.symbol :as sym]))

;; ── 环境操作 ──────────────────────────────
(defn- empty-env []
  {:defs   {}
   :ho-set #{}})

(defn- get-env [ctx]
  (get ctx :env (empty-env)))

(defn- add-def [env name lam]
  (-> env
      (assoc-in [:defs name] lam)
      (update :ho-set conj name)))

(defn- add-def-to-ctx [ctx name lam]
  (assoc ctx :env (add-def (get-env ctx) name lam)))



;; ── 多方法分派（返回 [new-node, new-ctx]）────
(defmulti eliminate-ho
          (fn [node depth ctx] (n/kind node)))

(defmethod eliminate-ho :literal   [node _ ctx] [node ctx])
(defmethod eliminate-ho :variable  [node _ ctx] [node ctx])

(defmethod eliminate-ho :define [node depth ctx]
  (let [name (n/define-name node)
        val  (n/define-val node)
        ho?  (-> node n/attrs :ho?)]
    (if (and val (n/lambda-node? val))
      (let [[new-val ctx1] (eliminate-ho val (inc depth) ctx)
            ctx2 (if ho? (add-def-to-ctx ctx1 name new-val) ctx1)]
        [(n/make-define name new-val (n/define-doc node)
                        (n/attrs node) (n/node-meta node) (n/parent node))
         ctx2])
      (let [[new-val ctx1] (if val (eliminate-ho val depth ctx) [val ctx])]
        [(n/make-define name new-val (n/define-doc node)
                        (n/attrs node) (n/node-meta node) (n/parent node))
         ctx1]))))

(defmethod eliminate-ho :call [node depth ctx]
  (let [max-depth (get ctx :ho-max-depth 20)
        fn-node   (n/call-fn node)
        [args ctx-args]
        (reduce (fn [[as ctx] arg]
                  (let [[new-arg new-ctx] (eliminate-ho arg (inc depth) ctx)]
                    [(conj as new-arg) new-ctx]))
                [[] ctx]
                (n/call-args node))
        env (get-env ctx-args)]
    (if (n/variable-node? fn-node)
      (let [fn-name (n/var-name fn-node)]
        (if (and (contains? (:ho-set env) fn-name) (< depth max-depth))
          (if-let [lam (get-in env [:defs fn-name])]
            (let [params  (n/lambda-params lam)
                  body    (n/lambda-body lam)
                  inlined (reduce (fn [b [p a]]
                                    (replace/replace-var b (n/var-name p) a))
                                  body
                                  (map vector params args))]
              (eliminate-ho inlined (inc depth) ctx-args))  ;; 递归内联
            [(n/make-call fn-node args (n/attrs node) (n/node-meta node) (n/parent node)) ctx-args])
          [(n/make-call fn-node args (n/attrs node) (n/node-meta node) (n/parent node)) ctx-args]))
      ;; 非变量调用（匿名函数调用等）留给 lambda-elim 处理
      (let [[new-fn ctx-fn] (eliminate-ho fn-node depth ctx-args)]
        [(n/make-call new-fn args (n/attrs node) (n/node-meta node) (n/parent node)) ctx-fn]))))

;; ── 容器节点：返回二元组并传递上下文 ────
(defmethod eliminate-ho :if [node depth ctx]
  (let [[test ctx1] (eliminate-ho (n/if-test node) depth ctx)
        [then ctx2] (eliminate-ho (n/if-then node) depth ctx1)
        [else ctx3] (if-let [e (n/if-else node)] (eliminate-ho e depth ctx2) [nil ctx2])]
    [(n/make-if test then else (n/attrs node) (n/node-meta node) (n/parent node)) ctx3]))

(defmethod eliminate-ho :block [node depth ctx]
  (let [[new-exprs final-ctx]
        (reduce (fn [[exprs ctx] expr]
                  (let [[new-expr new-ctx] (eliminate-ho expr depth ctx)]
                    [(conj exprs new-expr) new-ctx]))
                [[] ctx]
                (n/block-exprs node))]
    [(n/make-block new-exprs (n/attrs node) (n/node-meta node) (n/parent node)) final-ctx]))

(defmethod eliminate-ho :let [node depth ctx]
  (let [[new-bindings ctx1]
        (reduce (fn [[bnds ctx] [v e]]
                  (let [[new-v ctx'] (eliminate-ho v depth ctx)
                        [new-e ctx''] (eliminate-ho e depth ctx')]
                    [(conj bnds [new-v new-e]) ctx'']))
                [[] ctx]
                (n/let-bindings node))
        [new-body ctx2] (eliminate-ho (n/let-body node) depth ctx1)]
    [(n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node)) ctx2]))

(defmethod eliminate-ho :loop [node depth ctx]
  (let [[new-bindings ctx1]
        (reduce (fn [[bnds ctx] [v e]]
                  (let [[new-v ctx'] (eliminate-ho v depth ctx)
                        [new-e ctx''] (eliminate-ho e depth ctx')]
                    [(conj bnds [new-v new-e]) ctx'']))
                [[] ctx]
                (n/loop-bindings node))
        [new-body ctx2] (eliminate-ho (n/loop-body node) depth ctx1)]
    [(n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node)) ctx2]))

(defmethod eliminate-ho :lambda [node depth ctx]
  (let [[params ctx1] (reduce (fn [[ps ctx] p]
                                (let [[new-p ctx'] (eliminate-ho p depth ctx)]
                                  [(conj ps new-p) ctx']))
                              [[] ctx]
                              (n/lambda-params node))
        [body ctx2] (eliminate-ho (n/lambda-body node) depth ctx1)]
    [(n/make-lambda params body (n/lambda-captures node) (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     ctx2]))

(defmethod eliminate-ho :recur [node depth ctx]
  (let [[args ctx1] (reduce (fn [[as ctx] arg]
                              (let [[new-arg ctx'] (eliminate-ho arg depth ctx)]
                                [(conj as new-arg) ctx']))
                            [[] ctx]
                            (n/recur-args node))]
    [(n/make-recur args (n/attrs node) (n/node-meta node) (n/parent node)) ctx1]))

(defmethod eliminate-ho :while [node depth ctx]
  (let [[test ctx1] (eliminate-ho (n/while-test node) depth ctx)
        [body ctx2] (eliminate-ho (n/while-body node) depth ctx1)]
    [(n/make-while test body (n/attrs node) (n/node-meta node) (n/parent node)) ctx2]))

;; 数组特殊节点
(defmethod eliminate-ho :new-array [node depth ctx]
  (let [[size ctx1] (eliminate-ho (n/new-array-size node) depth ctx)]
    [(n/make-new-array size (n/node-meta node) (n/parent node)) ctx1]))
(defmethod eliminate-ho :aget [node depth ctx]
  (let [[target ctx1] (eliminate-ho (n/aget-target node) depth ctx)
        [idx ctx2] (eliminate-ho (n/aget-idx node) depth ctx1)]
    [(n/make-aget target idx (n/node-meta node) (n/parent node)) ctx2]))
(defmethod eliminate-ho :aset [node depth ctx]
  (let [[target ctx1] (eliminate-ho (n/aset-target node) depth ctx)
        [idx ctx2] (eliminate-ho (n/aset-idx node) depth ctx1)
        [val ctx3] (eliminate-ho (n/aset-val node) depth ctx2)]
    [(n/make-aset target idx val (n/node-meta node) (n/parent node)) ctx3]))
(defmethod eliminate-ho :alength [node depth ctx]
  (let [[target ctx1] (eliminate-ho (n/alength-target node) depth ctx)]
    [(n/make-alength target (n/node-meta node) (n/parent node)) ctx1]))

(defmethod eliminate-ho :default [node _ ctx] [node ctx])


(defn make-context
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:env               (empty-env)
     :frontend          frontend
     :ctx               compile-ctx
     :backend           backend
     :ho-max-depth      20   ; 内联深度限制
     :symbol-table      symbols
     :known-types       (sym/types-symbols symbols)}))

(defn process
  [ir2-roots context]
  (let [ctx (-> context
                (assoc :ho-max-depth (get context :ho-max-depth 20))
                (assoc :env (or (:env context) (empty-env))))
        ;; 1. 分析并标记高阶函数
        analyzed-roots (analyze/analyze ir2-roots)
        ;; 2. 内联，顺序传递上下文
        [new-roots _]
        (reduce (fn [[rs ctx] root]
                  (let [[new-root new-ctx] (eliminate-ho root 0 ctx)]
                    [(conj rs new-root) new-ctx]))
                [[] (assoc ctx :env (empty-env))]
                analyzed-roots)]
    new-roots))