(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.call
  "约束生成：:call 节点。
   候选函数类型由 env 统一解析，pass 不区分本地 / 全局。
   当实参全部具体且候选匹配时，直接确定返回类型。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :call [current-node env]
  (let [{fn-type        :type
         fn-node        :node
         fn-constraints :constraints
         fn-env         :env}
        (gen/gen-node* (n/call-fn current-node) env)
        fn-name     (when (= (n/kind fn-node) :variable)
                      (n/var-name fn-node))
        candidates  (cond
                      (and fn-type (t/fun-type? fn-type)) [fn-type]
                      fn-name                             (p/resolve-callees fn-env fn-name)
                      :else                               [])
        [arg-results final-env]
        (reduce (fn [[results current-env] arg]
                  (let [{:keys [env] :as r} (gen/gen-node* arg current-env)]
                    [(conj results r) env]))
                [[] fn-env]
                (n/call-args current-node))
        arg-types    (mapv :type arg-results)
        arg-nodes    (mapv :node arg-results)
        arg-constrs  (mapcat :constraints arg-results)
        args-concrete? (every? t/concrete? arg-types)
        matched-cand (when args-concrete?
                       (some (fn [cand]
                               (let [cand-args (take-while some? (map :arg (iterate :ret cand)))]
                                 (when (and (= (count cand-args) (count arg-types))
                                            (every? true? (map = cand-args arg-types)))
                                   cand)))
                             candidates))]
    (if matched-cand
      (let [ret-type (t/fun-return-type matched-cand (count arg-types))
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            ;; callee 类型是 TVar 时（多 arity 函数在 variable 阶段未解析），
            ;; 约束到匹配的候选
            fn-eq    (when (and fn-type (t/var-type? fn-type))
                       [(cons/make-cequal fn-type matched-cand)])]
        {:type        ret-type
         :node        (t/set-type! new-node ret-type)
         :constraints (concat fn-eq fn-constraints arg-constrs)
         :env         final-env})
      (let [ret-tv   (gen/fresh-tvar)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            extra    (if (seq candidates)
                       [(cons/make-coverload candidates arg-types ret-tv fn-type)]
                       [])]
        {:type        ret-tv
         :node        (t/set-type! new-node ret-tv)
         :constraints (concat extra fn-constraints arg-constrs)
         :env         final-env}))))