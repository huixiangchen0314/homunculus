(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  "约束生成：:call 节点。查找顺序：局部环境 → 符号表。
   当实参全部具体且候选匹配时，直接确定返回类型。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as t]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmethod gen/cg-node-raw :call [node context]
  (let [[_fn-tv fn-node fn-constrs fn-ctx] (gen/cg-node-raw (n/call-fn node) context)
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        env (u/env fn-ctx)
        ;; 1. 尝试从环境获取函数类型
        env-binding (when fn-name
                      (or (u/lookup-env fn-ctx fn-name)
                          (u/lookup-env fn-ctx (symbol fn-name))))
        env-fn-ty (when env-binding
                    (if (scheme/tscheme? env-binding)
                      (scheme/instantiate env-binding)
                      env-binding))
        ;; 2. 环境没有，则查符号表（支持重载）
        func-entry (when (and fn-name (not env-fn-ty))
                     (sym/entry->func (sym/lookup-in-tables fn-name (u/symbol-table fn-ctx))))
        ;; 3. 构造候选函数类型序列
        candidates (cond
                     env-fn-ty (if (t/fun-type? env-fn-ty) [env-fn-ty] [])
                     func-entry (mapv t/arity->tfun (sym/list-arities func-entry))
                     :else [])
        args (n/call-args node)
        ;; 顺序处理实参，累积上下文、类型、节点、约束
        [arg-results final-ctx]
        (reduce (fn [[results ctx] arg]
                  (let [[tv new-arg constrs new-ctx] (gen/cg-node-raw arg ctx)]
                    [(conj results {:tv tv :node new-arg :constrs constrs})
                     new-ctx]))
                [[] fn-ctx]
                args)
        arg-tys   (mapv :tv arg-results)
        arg-nodes (mapv :node arg-results)
        arg-constrs (mapcat :constrs arg-results)
        args-concrete? (every? t/concrete? arg-tys)
        ;; 尝试精确匹配（如果有候选且实参具体）
        matched-cand (when args-concrete?
                       (some (fn [cand]
                               (let [cand-args (take-while some? (map :arg (iterate :ret cand)))]
                                 (when (and (= (count cand-args) (count arg-tys))
                                            (every? true? (map = cand-args arg-tys)))
                                   cand)))
                             candidates))]
    (if matched-cand
      ;; 精确匹配，直接确定返回类型
      (let [ret-ty (t/fun-return-type matched-cand (count arg-tys))
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs node) (n/node-meta node) (n/parent node))]
        [ret-ty (t/set-type! new-node ret-ty) (concat fn-constrs arg-constrs) final-ctx])
      ;; 无法精确匹配，生成 COverload 或新变量
      (let [ret-tv (gen/fresh-tvar)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs node) (n/node-meta node) (n/parent node))]
        (if (seq candidates)
          [ret-tv
           (t/set-type! new-node ret-tv)
           (concat (list (c/make-coverload candidates arg-tys ret-tv new-node))
                   fn-constrs arg-constrs)
           final-ctx]
          ;; 无候选，返回新类型变量，无额外约束
          (let [tv (gen/fresh-tvar)]
            [tv (t/set-type! new-node tv) (concat fn-constrs arg-constrs) final-ctx]))))))