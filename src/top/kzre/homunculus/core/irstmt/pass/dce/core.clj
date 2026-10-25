(ns top.kzre.homunculus.core.irstmt.pass.dce.core
  "死代码消除——基于集合的活跃变量分析。

   每个语句的 live-in = (live-out - defs) ∪ uses。

   删除判断（在 live-out 上判定）：
   - var-decl：name ∉ live-ref-out → 删除（若 init 无副作用）
   - assign（简单变量）：target ∉ live-read-out → 删除
   - 其他语句：fx? → 保留

   删除语句时回滚贡献（返回 live-out 而非 live-in），
   让前驱看到准确的活需求，从而连锁删除无用的声明。

   if 分支合并：live-read 和 live-ref 分别 union。
   while 不动点：entry = live-out ∪ live-in(body, entry)，union 单调收敛。

   关键案例验证：

   (A) int x; x = 1; use(x);
       - use(x)   : live = ({x}, {x})
       - x = 1    : x ∈ live-read-out → 保留；after = ({}, {x})
       - int x    : x ∈ live-ref-out → 保留 ✓

   (B) int x; x = 1;                （x 未使用）
       - x = 1    : x ∉ live-read-out → 删除；回滚返回 ({}, {})
       - int x    : x ∉ live-ref-out → 删除 ✓

   (C) (if true 1 (if false 2 3)) 的 ifval 链条
       - 所有 ifval 被返回结构 use，live-read/live-ref 传播正确
       - 声明和赋值全部保留 ✓"

  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.dce.env :as env]))

(defonce fx-nodes
         #{:record :field :param :method :protocol :protocol-impl
           :ns :try :catch :throw :function})

(defn fx? [node]
  (and node
       (or (contains? fx-nodes (ast/kind node))
           (get-in node [:attrs :fx?]))))

(defn set-fx [node b]
  (assoc-in node [:attrs :fx?] (or (fx? node) (boolean b))))

(defmulti eliminate-node (fn [node _env] (ast/kind node)))
(defn elim-node [node env]
  (eliminate-node node env))

(defn walk
  [node env]
  (let [[new-node new-env]
        (ast/rreduce-children node eliminate-node env)]
    [(set-fx new-node (some fx? (ast/children new-node)))
     new-env]))

(defmethod eliminate-node :default [node env]
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
  ;; 先处理声明（新变量上下文），再处理 init
  (let [env1 (env/decl-var env (:name node))
        [val' env'] (if-let [v (:val node)]
                      (eliminate-node v env1)
                      [nil env1])]
    [(set-fx (assoc node :val val') (fx? val')) env']))

(defmethod eliminate-node :assign
  [node env]
  (let [var (:var node)]
    (if (= :variable (ast/kind var))
      ;; 简单变量：target 只 def-var（kill 旧值，保留引用）
      (let [env1 (env/def-var env (:name var))
            [val' env'] (eliminate-node (:val node) env1)]
        [(set-fx (assoc node :val val') (fx? val')) env'])
      ;; 复合左值（aget / member-access）：写内存，副作用，target 内部变量被读
      (let [[var' env1] (eliminate-node var env)
            [val' env'] (eliminate-node (:val node) env1)]
        [(set-fx (assoc node :var var' :val val') true) env']))))

(defmethod eliminate-node :aset
  [node env]
  (let [[val-node env1]    (eliminate-node (:val node) env)
        [idx-node env2]    (eliminate-node (:idx node) env1)
        [target-node env3] (eliminate-node (:target node) env2)]
    [(set-fx (assoc node :target target-node :idx idx-node :val val-node) true)
     env3]))

(defn- living?
  "语句 stmt 在 live-out 下是否存活。"
  [stmt live-out]
  (or (fx? stmt)
      (case (ast/kind stmt)
        :var-decl
        (env/live-ref? live-out (:name stmt))
        :assign
        (let [t (:var stmt)]
          (and (= :variable (ast/kind t))
               (env/live-read? live-out (:name t))))
        true)))

(defmethod eliminate-node :block
  [node env]
  (let [stmts (:stmts node)
        ret   (:ret node)
        [ret' env1] (if ret
                      (eliminate-node ret env)
                      [ret env])
        [kept live-in]
        (reduce (fn [[ss live] stmt]
                  (let [[stmt' after] (eliminate-node stmt live)]
                    (if (living? stmt' live)
                      [(conj ss stmt') after]
                      ;; 删除：回滚贡献，前驱的 live-out = 本条 stmt 的 live-out
                      [ss live])))
                [[] env1]
                (reverse stmts))]
    [(set-fx (assoc node :stmts (vec (reverse kept)) :ret ret')
             (or (seq kept) ret'))
     live-in]))

(defmethod eliminate-node :if
  [node env]
  (let [[then' env-then] (eliminate-node (:then node) env)
        [else' env-else] (if-let [e (:else node)]
                           (eliminate-node e env)
                           [nil env])
        merged (env/merge-branches env-then env-else)
        [test' env-test] (eliminate-node (:test node) merged)
        has-fx (or (fx? test') (fx? then') (fx? else'))]
    [(set-fx (assoc node :test test' :then then' :else else') has-fx)
     env-test]))

(defmethod eliminate-node :while
  [node env]
  (let [test (:test node)
        body (:body node)
        ;; 不动点：entry = env ∪ live-in(body, entry)
        ;;   env 项：0 次迭代路径（循环后的 live 也是入口 live）
        ;;   body-in 项：N 次迭代路径（下一轮迭代的 live-out）
        [body' entry]
        (loop [entry env n 0]
          (when (> n 1000)
            (throw (ex-info "while fixpoint did not converge" {:while node})))
          (let [[body-in' body-in] (eliminate-node body entry)
                new-entry (env/merge-branches env body-in)]
            (if (env/same-live? entry new-entry)
              [body-in' entry]
              (recur new-entry (inc n)))))
        ;; test 在 body 之前执行，从循环入口的 live 出发
        [test' test-env] (eliminate-node test entry)
        has-fx (or (fx? test') (fx? body'))]
    [(set-fx (assoc node :test test' :body body') has-fx)
     test-env]))

(defn elim [nodes init-env]
  (mapv (fn [node]
          (case (ast/kind node)
            (:function :record)
            (first (eliminate-node node init-env))
            node))
        nodes))