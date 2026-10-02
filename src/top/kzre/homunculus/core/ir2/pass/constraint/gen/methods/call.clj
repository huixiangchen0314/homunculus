(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.call
  "约束生成：:call 节点。查找顺序：局部环境 → 符号表。
   当实参全部具体且候选匹配时，直接确定返回类型。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.type :as t]
    [top.kzre.homunculus.core.symbol :as sym]))

(defmethod gen/gen-node* :call [current-node context]
  (let [{fn-node        :node
         fn-constraints :constraints
         fn-env         :env}
        (gen/gen-node* (n/call-fn current-node) context)
        fn-name     (when (= (n/kind fn-node) :variable)
                      (n/var-name fn-node))
        ;; 1. 尝试从环境获取函数类型
        env-binding (when fn-name
                      (or (u/lookup-env fn-env fn-name)
                          (u/lookup-env fn-env (symbol fn-name))))
        env-fn-type (when env-binding
                      (if (scheme/tscheme? env-binding)
                        (scheme/instantiate env-binding)
                        env-binding))
        ;; 2. 环境没有，则查符号表（支持重载）
        func-entry  (when (and fn-name (not env-fn-type))
                      (sym/entry->func
                        (sym/lookup-in-tables fn-name (u/symbol-table fn-env))))
        ;; 3. 构造候选函数类型序列
        candidates  (cond
                      env-fn-type (if (t/fun-type? env-fn-type) [env-fn-type] [])
                      func-entry  (mapv t/arity->tfun (sym/list-arities func-entry))
                      :else       [])
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
      ;; 精确匹配，直接确定返回类型
      (let [ret-type (t/fun-return-type matched-cand (count arg-types))
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))]
        {:type        ret-type
         :node        (t/set-type! new-node ret-type)
         :constraints (concat fn-constraints arg-constrs)
         :env         final-env})
      ;; 无法精确匹配
      (let [ret-tv   (gen/fresh-tvar)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            extra    (if (seq candidates)
                       [(cons/make-coverload candidates arg-types ret-tv)]
                       [])]
        {:type        ret-tv
         :node        (t/set-type! new-node ret-tv)
         :constraints (concat extra fn-constraints arg-constrs)
         :env         final-env}))))