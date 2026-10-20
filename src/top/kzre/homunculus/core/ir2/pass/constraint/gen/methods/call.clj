(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.call
  "约束生成：:call 节点。
   候选函数类型由 env 统一解析，pass 不区分本地 / 全局。
   当实参全部具体且候选匹配时，直接确定返回类型。

   Arity 检查：
   - 先在投递约束前过滤掉 arity 不匹配的候选（普通 :call 是完全应用，
     不支持部分应用）。
   - 若所有候选 arity 都不匹配 → 直接报 arity-mismatch 错误，
     不投递不可能满足的约束。"
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defn- cand-arity
  "候选函数的参数数量——沿 :ret 链展开，遇到非 TFun 停止。"
  [cand]
  (count (take-while some? (map :arg (iterate :ret cand)))))

(defn- cand-param-types
  "候选函数的参数类型列表。"
  [cand]
  (take-while some? (map :arg (iterate :ret cand))))

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
        arg-count    (count arg-results)
        arg-types    (mapv :type arg-results)
        arg-nodes    (mapv :node arg-results)
        arg-constrs  (mapcat :constraints arg-results)
        args-concrete? (every? t/concrete? arg-types)

        ;; ── Arity 过滤 ──
        ;; 完全应用：候选的参数数量必须等于实参数量。
        ;; 不匹配的候选不投递约束——它们的 arity 本身就不对。
        arity-compatible (filterv #(= (cand-arity %) arg-count) candidates)

        ;; ── 类型完全匹配 ──
        matched-cand
        (when (and args-concrete? (seq arity-compatible))
          (some (fn [cand]
                  (let [cand-args (cand-param-types cand)]
                    (when (every? true? (map = cand-args arg-types))
                      cand)))
                arity-compatible))]

    (cond
      ;; ── 完全匹配：直接确定返回类型 ──
      matched-cand
      (let [ret-type (t/fun-return-type matched-cand arg-count)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            fn-eq    (when (and fn-type (t/var-type? fn-type))
                       [(cons/make-cequal fn-type matched-cand)])]
        {:type        ret-type
         :node        (t/set-type! new-node ret-type)
         :constraints (concat fn-eq fn-constraints arg-constrs)
         :env         final-env})

      ;; ── 无候选：未定义函数，交给 check 阶段处理 ──
      (empty? candidates)
      (let [ret-tv   (gen/fresh-tvar)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            ret-eq
            (when (p/call-driven-inference? env)
              [(cons/make-cequal
                 (t/fun-return-type (first arity-compatible) arg-count)
                 ret-tv)])]
        {:type        ret-tv
         :node        (t/set-type! new-node ret-tv)
         :constraints (concat fn-constraints arg-constrs ret-eq)
         :env         final-env})

      ;; ── 有候选但 arity 全不匹配：直接报错 ──
      (empty? arity-compatible)
      (throw (err/compile-error
               :arity-mismatch
               (str "no candidate accepts " arg-count " arguments"
                    (when fn-name (str " for " fn-name)))
               {:node       current-node
                :arg-count  arg-count
                :candidates (mapv (fn [c]
                                    {:arity (cand-arity c)
                                     :type  c})
                                  candidates)}))

      ;; ── Arity 匹配但类型未完全确定：投递约束（仅 arity 兼容的候选）──
      :else
      (let [ret-tv (gen/fresh-tvar)

            new-node (n/make-call fn-node
                                  (vec arg-nodes)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))
            extra    [(cons/make-coverload arity-compatible arg-types ret-tv fn-type
                                           :call-driven-interface?  (p/call-driven-inference? env)
                                           :data new-node)]
            ;; 单候选 + call-driven-inference 开启：
            ;; 只有唯一候选，返回值必然是它的返回类型——
            ;; 显式约束，不依赖 COverload 单候选分支的内部绑定。
            ret-eq
            (when (and
                    (p/call-driven-inference? env)
                    (= 1 (count arity-compatible)))
              [(cons/make-cequal
                 (t/fun-return-type (first arity-compatible) arg-count)
                 ret-tv)])]
        {:type        ret-tv
         :node        (t/set-type! new-node ret-tv)
         :constraints (concat fn-constraints arg-constrs ret-eq extra)
         :env         final-env}))))