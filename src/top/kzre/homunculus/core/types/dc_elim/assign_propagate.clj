(ns top.kzre.homunculus.core.types.dc-elim.assign-propagate
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [clojure.set :as set]))

(defrecord Env [ctx defs substs use-counts])
(defn make-env [ctx] (->Env ctx {} {} {}))
(defn env-def [env var-name] (get-in env [:defs var-name]))
(defn env-set-def [env var-name expr] (assoc-in env [:defs var-name] expr))
(defn env-add-use [env var-name] (update-in env [:use-counts var-name] (fnil inc 0)))
(defn env-set-use-count [env var-name cnt] (update-in env [:use-counts var-name] cnt))
(defn env-reset-def [env var-name expr]
  (let [env1 (env-set-def env var-name expr)
        env2 (env-set-use-count env1 var-name 0)]
    env2))
(defn env-remove-use [env var-name]
  (update-in env [:use-counts var-name] (fn [c] (max 0 (dec (or c 0))))))
(defn env-use-count [env var-name] (get-in env [:use-counts var-name] 0))
(defn env-substs [env var-name] (get-in env [:substs var-name]))


(defn env-has-subst?
  "检查 var-name 是否作为替换目标存在于 substs 中"
  [env var-name]
  (some (fn [targets] (contains? targets var-name)) (vals (:substs env))))

(defn env-add-subst
  "添加映射 src -> target，自动扁平化替换链并清理被合并的中间映射。
   如果 target 自身在 substs 中已有映射，则 src 直接指向 target 的目标，
   并删除 target 的条目，避免间接替换。"
  [env src target]
  (let [targets (get-in env [:substs target])
        ;; 确定 src 要指向的最终目标集合
        final-targets (if targets
                        targets          ; target 已有映射，直接使用其目标集合
                        #{target})      ; target 无映射，最终目标就是自己
        ;; 如果 target 原本有映射，则从 substs 中移除它（因为 src 现在直接覆盖了它）
        env' (if targets
               (update env :substs dissoc target)
               env)
        ;; 合并 src 已有的映射
        existing (get-in env' [:substs src] #{})
        merged (set/union existing final-targets)]
    (assoc-in env' [:substs src] merged)))

(defn env-clear-substs [env]
  (assoc env :substs {}))
(defn env-remove-subst [env src]
  (update env :substs dissoc src))
(defn env-remove-subst-containing
  "移除 substs 中所有值集合包含 var-name 的条目（即 var-name 作为目标的所有映射）"
  [env var-name]
  (update env :substs
          (fn [substs]
            (into {} (remove (fn [[_ targets]] (contains? targets var-name)) substs)))))

(defmulti propagate-node
          (fn [node _env] (ir2/kind node)))

(defn node-io?
  [node]
  (get-in node [:attrs :io?] false))

(defn walk [node env]
  (ir2/reduce-children node propagate-node env))

(defmethod propagate-node :default [node env]
 (walk node env))

(defn reduce-exprs
  [exprs env]
  (let [
        outer-subst-keys (set (keys (:substs env)))
        ;; 正向遍历一次积累环境
        [forward-nodes env-forward]
        (reduce
          (fn [[nodes e] n]
            ;; 记录栈
            (let [node-kind (ir2/kind n)]
              (case node-kind
                :define
                (let [var-name (n/define-name n)
                      val-node (n/define-val n)
                      ;; 后序
                      [val-node e'] (when val-node (propagate-node val-node e))
                      ;; 重置定义
                      env1 (env-reset-def e' var-name val-node)
                      val-var? (when val-node (= :variable (ir2/kind val-node)))
                      ;; 添加替换
                      env2
                      (if val-var?
                        (env-add-subst env1 (n/var-name val-node) var-name)
                        env1)
                      ]
                  [n env2])
                :assign
                (let [lhs (n/assign-var n)
                      rhs (n/assign-val n)]
                  (if (n/variable-node? lhs)
                    (let [[rhs' e'] (propagate-node rhs e)
                          var-name (n/var-name lhs)
                          e'' (env-set-def e' var-name rhs')
                          e''' (if (and rhs' (= :variable (ir2/kind rhs')))
                                 (env-add-subst e'' (n/var-name rhs') var-name)
                                 e'')]
                      [(conj nodes (assoc n :val rhs')) e'''])
                    (let [[n' e'] (propagate-node n e)]
                      [(conj nodes n') e'])))
                ;; default recur
                (let [[new-node new-env] (walk n e)]
                  [(conj nodes new-node)
                   new-env]))))
          [[] env]
          exprs)
        ;; 反向遍历一次执行替换
        [backward-nodes env-backward]
        (reduce
          (fn [[nodes e] n]

            (let [node-kind (ir2/kind n)]
              (case node-kind
                :define
                ;; 如果存在替换且初始值无副作用
                ;; 就删除本定义，
                ;; 否则删除替换
                (let [var-name (n/define-name n)
                      val-node (n/define-val n)
                      substs (env-substs e var-name)]
                  (if (seq substs)
                    ;; 如果存在替换，就构建目标的定义，并移除替换
                    (let [def-nodes (mapv #(n/make-define % nil {} nil) substs)
                          new-defs (if val-node
                                     (conj def-nodes val-node)
                                     def-nodes)
                          block (n/make-block new-defs {} nil)
                          e1 (env-remove-subst e var-name)]
                      [block e1])
                    (if (env-has-subst? e var-name)
                      [val-node
                       e]
                      [(conj nodes n) e])))
                :assign
                (let [lhs (n/assign-var n)
                      var-name (n/var-name lhs)
                      val-node (n/assign-val n)   ;; 正向遍历已处理过的右值
                      substs (when var-name (env-substs e var-name))]
                  (if (seq substs)
                    ;; 存在替代目标：生成赋值块，消除当前赋值
                    (let [assigns (mapv (fn [v] (n/make-assign (n/make-variable v nil nil) val-node nil nil)) substs)
                          block (n/make-block assigns nil nil)
                          e' (-> e
                                 (env-remove-subst var-name)
                                 (env-remove-subst-containing var-name))]
                      [block e'])
                    (if (and var-name (env-has-subst? e var-name))
                      ;; 自身是其他变量的目标，消除赋值，并移除相关映射
                      [val-node (env-remove-subst-containing e var-name)]
                      ;; 保留原赋值（可能右值已更新）
                      [(conj nodes n) e])))

                :else
                (let [[new-node new-env] (propagate-node n e)]
                  [(conj nodes new-node)
                   new-env]))))
          [[] env-forward]
          (reverse forward-nodes))
        backward-nodes (reverse backward-nodes)
        ;; ── 处理挂起的定义（作用域结束前仍未声明的替代目标） ──
        inner-substs (apply dissoc (:substs env-backward) outer-subst-keys) ; 仅内部新增的映射
        remaining-targets (set (apply concat (vals inner-substs)))
        def-nodes (mapv #(n/make-define % nil {} nil) remaining-targets)
        final-nodes (if (seq def-nodes)
                      (into def-nodes backward-nodes)   ; 声明在前
                      backward-nodes)
        ;; 恢复环境：保留外部 substs，清除内部新增的
        env-final (assoc env-backward :substs (select-keys (:substs env-backward) outer-subst-keys))
        ]
    [final-nodes
     env-final]))

(defmethod propagate-node :block
 [node env]
 (let [exprs (n/block-exprs node)
       [nodes env'] (reduce-exprs exprs env)]
   [(assoc node :exprs nodes)
    env']))


;; ── 入口 ──
(defn propagate-nodes [nodes ctx]
  (let [init-env (make-env ctx)
        [new-nodes _]
        (reduce (fn [[ns env] n]
                  (let [[n' e'] (propagate-node n env)]
                    [(conj ns n') e']))
                [[] init-env]
                nodes)]
    new-nodes))