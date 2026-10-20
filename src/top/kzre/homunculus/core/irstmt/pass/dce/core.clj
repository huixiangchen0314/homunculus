(ns top.kzre.homunculus.core.irstmt.pass.dce.core
  "死代码消除——基于计数 env 的内联活跃性分析。

   block 逆序遍历，env 传播：
   - 处理每个语句时，传入的 env 就是该语句的 live-out
   - 返回的 env 是该语句的 live-in（内部 def/use 已累积）
   - living? 检查：语句定义的变量是否在 live-out 中被 use

   while 的不动点：
   - 循环可能执行 0 次：循环入口的 live 至少包含循环后的 live（env）
   - 循环可能执行 N 次：body 的 live-in 成为下一次迭代的 live-out
   - 迭代直到 entry = env ∪ live-in(body, entry) 收敛"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.dce.env :as env]))

;; ── fx? 传播 ──
(defonce fx-nodes
         #{:record :field :param :method :protocol :protocol-impl
           :ns :try :catch :throw :function})

(defn fx? [node]
  (and node
       (or (contains? fx-nodes (ast/kind node))
           (get-in node [:attrs :fx?]))))

(defn set-fx [node b]
  (assoc-in node [:attrs :fx?] (or (fx? node) (boolean b))))

;; ── 分派 ──
(defmulti eliminate-node (fn [node _env] (ast/kind node)))

(defn walk
  [node env]
  (let [[new-node new-env]
        (ast/rreduce-children node eliminate-node env)]
    [(set-fx new-node (some fx? (ast/children new-node)))
     new-env]))

;; ── 默认：递归所有子节点 ──
(defmethod eliminate-node :default
  [node env]
  (walk node env))

(defmethod eliminate-node :call
  [node env]
  (let [[node' env'] (walk node env)
        fn-name (some-> node' :fn :name)
        has-fx  (if fn-name
                  (env/fn-fx? env fn-name)
                  (some fx? (ast/children node')))]
    [(set-fx node' has-fx) env']))

(defmethod eliminate-node :variable
  [node env]
  [node (env/use-var env (:name node))])

(defmethod eliminate-node :var-decl
  [node env]
  (let [env1 (env/def-var env (:name node))
        [val' env'] (if-let [v (:val node)]
                      (eliminate-node v env1)
                      [nil env1])]
    [(set-fx (assoc node :val val') (fx? val')) env']))

(defmethod eliminate-node :assign
  [node env]
  (let [var-name (some-> node :var :name)
        env1 (if var-name (env/def-var env var-name) env)
        [val' env'] (eliminate-node (:val node) env1)]
    [(set-fx (assoc node :val val') (fx? val')) env']))

(defmethod eliminate-node :aset
  [node env]
  (let [[val-node env1]    (eliminate-node (:val node) env)
        [idx-node env2]    (eliminate-node (:idx node) env1)
        [target-node env3] (eliminate-node (:target node) env2)
        new-node (set-fx
                   (assoc node :target target-node :idx idx-node :val val-node)
                   true)]
    (if (or (fx? target-node) (fx? idx-node) (fx? val-node))
      [new-node env3]
      ;; 特殊优化：target 是简单变量且之后不被 use → 死写可删
      (if (and (= :variable (ast/kind target-node))
               (zero? (env/use-count env2 (:name target-node))))
        [nil env]
        [new-node env3]))))

(defn- living?
  "语句 stmt 在 live-out 下是否存活：
   1. 有副作用（fx?）
   2. 语句定义的变量在 live-out 中被 use

   live-out: 语句执行之后的 live 状态（逆序遍历中传入的 env）
   after:    eliminate-node 处理该语句后返回的 env（def/use 已反映）"
  [stmt live-out after]
  (or (fx? stmt)
      (let [defs (env/new-defs after live-out)]
        (boolean (some #(pos? (env/use-count live-out %)) defs)))))

(defmethod eliminate-node :block
  [node env]
  (let [stmts (:stmts node)
        ret   (:ret node)
        [ret' env1] (if ret
                      (eliminate-node ret env)
                      [nil env])
        [kept _]
        (reduce (fn [[ss live] stmt]
                  (let [[stmt' after] (eliminate-node stmt live)]
                    (if (living? stmt' live after)
                      [(conj ss stmt') after]
                      [ss after])))
                [[] env1]
                (reverse stmts))]
    [(set-fx (assoc node :stmts (vec (reverse kept)) :ret ret')
             (or (seq kept) ret'))
     env]))

(defmethod eliminate-node :if
  [node env]
  (let [test (:test node)
        then (:then node)
        else (:else node)
        ;; 分支独立从 if 的 live-out（env）出发，互不污染
        [then' env-then] (eliminate-node then env)
        [else' env-else] (if else
                           (eliminate-node else env)
                           [nil env])
        merged (env/merge-max env-then env-else)
        ;; test 在 if 之前执行，从合并后的 live 出发
        [test' env-test] (eliminate-node test merged)
        has-fx (or (fx? test') (fx? then') (fx? else'))]
    [(set-fx (assoc node :test test' :then then' :else else') has-fx)
     env-test]))

(defmethod eliminate-node :while
  [node env]
  (let [test (:test node)
        body (:body node)
        ;; 不动点：entry = env ∪ live-in(body, entry)
        ;;   env 项：0 次迭代的路径（循环后的 live 也是入口 live）
        ;;   body-in 项：N 次迭代的路径（下一轮迭代的 live-out）
        [body' entry]
        (loop [entry env
               n     0]
          (when (> n 1000)
            (throw (ex-info "while liveness fixpoint did not converge"
                            {:while node})))
          (let [[body-in' body-in] (eliminate-node body entry)
                new-entry (env/merge-max env body-in)]
            (if (env/same-counts? entry new-entry)
              [body-in' entry]
              (recur new-entry (inc n)))))
        ;; test 在 body 之前执行，从循环入口的 live 出发
        [test' test-env] (eliminate-node test entry)
        has-fx (or (fx? test') (fx? body'))]
    [(set-fx (assoc node :test test' :body body') has-fx)
     test-env]))

(defn elim
  [nodes env]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :record)
            (first (eliminate-node node env))
            node))
        nodes))