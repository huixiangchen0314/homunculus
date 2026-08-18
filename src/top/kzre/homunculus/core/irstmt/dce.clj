(ns top.kzre.homunculus.core.irstmt.dce
  "死代码消除"
  (:require
   [top.kzre.homunculus.core.irstmt.ast :as ast]
   [top.kzre.homunculus.internal.protocol :as proto]
   [top.kzre.homunculus.internal.symbol :as sym]
   [top.kzre.homunculus.core.ir2.node :as n]))

(defrecord Env [ctx
                def-counts
                use-counts])
(defn make-env
  [ctx]
  (->Env ctx {} {}))

(defn env-use-var
  "增加变量 var-name 的使用次数，返回更新后的 env。"
  [env var-name]
  (update-in env [:use-counts var-name] (fnil inc 0)))

(defn env-use-count
  "查询变量 var-name 的使用次数。"
  [env var-name]
  (get-in env [:use-counts var-name] 0))

(defn env-has-use? [env var-name]
  (contains? (:use-counts env) var-name))

(defn env-def-var
  [env var-name]
  (-> env
      (update-in [:def-counts var-name] (fnil inc 0))
      (update-in [:use-counts var-name] (constantly 0))))

(defn env-def-count
  [env var-name]
  (get-in env [:def-counts var-name] 0))



(defn fn-fx?
  [fn-name env]
  (let [ctx (:ctx env)
        tbl (proto/symbol-table ctx)
        entry (sym/lookup-sym tbl fn-name)]
    (sym/func-fx? entry)))

(defonce fx-nodes #{:record :field :param :method :protocol :protocol-impl :ns :try :catch :throw :function})

(defn fx? [node]
  (or (contains? fx-nodes (ast/kind node))
      (get-in node [:attrs :fx?])))

(defn set-fx [node b]
  (assoc-in node [:attrs :fx?] (or (fx? node) (boolean b))))


(defmulti eliminate-node (fn [node _env]
                           (ast/kind node)))

(defn walk
  [node env]
  (let [[new-node new-env]
        (ast/rreduce-children node eliminate-node env)]
    [(set-fx new-node (some fx? (ast/children new-node)))
     new-env]))

;; 默认：递归所有子节点,传播 fx?
(defmethod eliminate-node :default [node env]
  (walk node env))

(defmethod eliminate-node :call
 [node env]
 (let [[node' env'] (walk node env)
       callee (n/call-fn node')
       fn-name (n/var-name callee)
       fx? (fn-fx? fn-name env)]
   [(set-fx node' fx?)
    env']))

(defmethod eliminate-node :variable
  [node env]
  (let [var-name (n/var-name node)
        [node' env'] (walk node env)]
    [node' (env-use-var env' var-name)]))

(defmethod eliminate-node :var-decl [node env]
  (let [env (env-def-var env (:name node))]
    (if-let [val (:val node)]
      (let [[val' env'] (eliminate-node val env)]
        [(set-fx (assoc node :val val') (fx? val')) env'])
      [node env])))

(defmethod eliminate-node :assign [node env]
  (let [var (:var node)
        var-name (:name var)
        env (env-def-var env var-name)
        [val' env'] (eliminate-node (:val node) env)]
    [(set-fx (assoc node :val val') (fx? val')) env']))

(defmethod eliminate-node :aset
  [node env]
  (let [[val-node env1] (eliminate-node (:val node) env)
        [idx-node env2] (eliminate-node (:idx node) env1)
        [target-node env3] (eliminate-node (:target node) env2)]
    (if (or (fx? idx-node)
            (fx? val-node)
            (fx? target-node))
      [(set-fx
         (assoc node :target target-node :idx idx-node :val val-node)
         true)
       env3]
      (if (= :variable (ast/kind target-node))
        (let [var-name (:name target-node)
              use-count (env-use-count env2 var-name)]
          (if (pos? use-count)
            [(set-fx
               (assoc node :target target-node :idx idx-node :val val-node)
               true)
             env3]
            [nil env]))
        [(set-fx
           (assoc node :target target-node :idx idx-node :val val-node)
           true)
         env3]))))

(defn living-node?
  "语句存活条件：
   1. 有副作用 (fx?)
   2. 活跃变量"
  [node post-env pre-env]
  (if node
    (or
      (fx? node)
      (case (ast/kind node)
        :var-decl
        (let [var-name (:name node)]
          (env-has-use? post-env var-name))
        (let [pre-defs  (:def-counts pre-env)
              post-defs (:def-counts post-env)
              ;; 该语句新定义的变量（逆序遍历：pre > post）
              new-defs (into #{}
                             (filter (fn [v]
                                       (> (get pre-defs v 0)
                                          (get post-defs v 0)))
                                     (keys pre-defs)))]
          (boolean
            (some (fn [v]
                    (pos? (env-use-count post-env v)))
                  new-defs)))))
    false))

(defmethod eliminate-node :block
  [node env]
 (let [stmts (:stmts node)
       ret (:ret node)
       [ret' env1]
       (if ret
         (eliminate-node ret env)
         [ret env])

       [living-stmts env2]
       (reduce (fn [[ss e] s]
                 (let [[s' e'] (eliminate-node s e)]
                   (if (living-node? s' e e')
                     [(conj ss s')
                      e']
                     [ss e])))
               [[] env1]
               (reverse stmts))]
   [(set-fx
      (assoc node :stmts (reverse living-stmts)
                  :ret ret')
      (or (seq living-stmts) ret'))
    env2]))

(defmethod eliminate-node :if
  [node env]
  ;; 先逆序处理：分支合并，再 test
  (let [[then' env-then] (eliminate-node (:then node) env)
        [else' env-else] (if-let [e (:else node)]
                           (eliminate-node e env)
                           [nil env])
        merged-env (assoc env
                     :def-counts (merge-with + (:def-counts env-then) (:def-counts env-else))
                     :use-counts (merge-with + (:use-counts env-then) (:use-counts env-else)))
        [test' env-test] (eliminate-node (:test node) merged-env)
        fx-node? (or (fx? test') (fx? then') (fx? else'))]
    (if (not fx-node?)
      ;; 无副作用：使用逆序结果
      [(set-fx (assoc node :test test' :then then' :else else') fx-node?)
       env-test]
      ;; 有副作用：改为正序遍历（先 test，再分支）
      (let [[test-pos env-pos] (eliminate-node (:test node) env)
            [then-pos env-then-pos] (eliminate-node (:then node) env-pos)
            [else-pos env-else-pos] (if-let [e (:else node)]
                                      (eliminate-node e env-pos)
                                      [nil env-pos])
            merged-pos-env (assoc env-pos
                             :def-counts (merge-with + (:def-counts env-then-pos) (:def-counts env-else-pos))
                             :use-counts (merge-with + (:use-counts env-then-pos) (:use-counts env-else-pos)))]
        [(set-fx (assoc node :test test-pos :then then-pos :else else-pos) fx-node?)
         merged-pos-env]))))

(defmethod eliminate-node :while
  [node env]
  (let [[node1 env'] (walk node env)]
    (if (fx? node1)
      (let [[test env1] (eliminate-node (:test node) env)
            [body env2] (eliminate-node (:body node) env1)]
        [(set-fx
           (assoc node :test test :body body)
           true)
         env2])
      [node1 env'])))

(defn elim-nodes [nodes ctx]
  (let [init-env (make-env ctx)]
    (mapv (fn [node]
            (case (ast/kind node)
              (:function :record)
              (first (eliminate-node node init-env))
              node))
          nodes)))