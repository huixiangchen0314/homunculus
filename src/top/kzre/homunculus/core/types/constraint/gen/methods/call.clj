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

;; ── 向量序列操作处理 ──────────────────────
(defn- handle-vector-op
  "处理 first, rest, conj 等向量操作，生成类型约束并返回四元组。"
  [fn-name node context fn-node fn-constrs fn-ctx]
  (let [args (n/call-args node)
        [coll-node elem-node] (if (= fn-name 'conj) args [(first args) nil])
        ;; 推导集合参数
        [coll-tv coll-node coll-constr coll-ctx] (gen/cg-node-raw coll-node fn-ctx)
        _ (when (and (nil? coll-tv) (not (t/var-type? coll-tv)))
            (throw (ex-info (str fn-name " requires a typed collection") {:node node})))
        is-homog (t/vec-type? coll-tv)
        is-hetero (t/hetero-vec? coll-tv)
        _ (when (and (not is-homog) (not is-hetero))
            (throw (ex-info (str fn-name " requires a vector type") {:node node})))]
    (if (= fn-name 'first)
      ;; ── first ──
      (let [elem-ty (if is-homog
                      (t/vec-element-type coll-tv)
                      (first (t/hetero-vec-types coll-tv)))
            ret-tv  (or elem-ty (gen/fresh-tvar))
            ceq     (when elem-ty (c/make-cequal ret-tv elem-ty))
            new-node (n/make-call fn-node [coll-node] (n/attrs node) (n/node-meta node) (n/parent node))]
        [ret-tv (t/set-type! new-node ret-tv) (concat coll-constr (when ceq [ceq])) coll-ctx])

      ;; ── rest 或 conj ──
      (let [old-len (when is-homog (t/vec-size coll-tv))
            old-elem (when is-homog (t/vec-element-type coll-tv))
            ;; 推导新元素（conj 时才有）
            [elem-tv elem-node elem-constr elem-ctx] (if elem-node
                                                       (gen/cg-node-raw elem-node coll-ctx)
                                                       [nil nil nil coll-ctx])
            new-len-tv (when is-homog (gen/fresh-tvar))
            delta (if (= fn-name 'conj) 1 -1)
            ;; 同构向量：添加长度约束 CVecLenAdd
            len-constr (when is-homog
                         (c/make-cveclenadd new-len-tv old-len delta))
            ;; 异构向量：直接构造结果类型
            hetero-ret (when is-hetero
                         (if (= fn-name 'conj)
                           (t/make-hetero-vec (conj (vec (t/hetero-vec-types coll-tv)) elem-tv))
                           (t/make-hetero-vec (rest (t/hetero-vec-types coll-tv)))))
            ;; 最终结果类型
            ret-ty (if is-homog
                     (t/make-tvec old-elem new-len-tv)
                     hetero-ret)
            ret-tv (gen/fresh-tvar)
            type-ceq (c/make-cequal ret-tv ret-ty)
            ;; conj 还需要约束新元素类型与向量元素类型一致
            elem-ceq (when (and (= fn-name 'conj) is-homog)
                       (c/make-cequal elem-tv old-elem))
            new-node (n/make-call fn-node (if elem-node [coll-node elem-node] [coll-node])
                                  (n/attrs node) (n/node-meta node) (n/parent node))
            final-ctx (if elem-ctx elem-ctx coll-ctx)]
        [ret-tv (t/set-type! new-node ret-tv)
         (concat coll-constr elem-constr (when len-constr [len-constr]) (when type-ceq [type-ceq]) (when elem-ceq [elem-ceq]))
         final-ctx]))))

;; ── 普通函数调用处理 ──────────────────────
(defn- handle-ordinary-call
  "处理普通函数调用，查找环境、符号表，生成重载约束。"
  [fn-name node context fn-node fn-constrs fn-ctx]
  (let [env (u/env fn-ctx)
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

;; ── 主方法 ──────────────────────────────
(defmethod gen/cg-node-raw :call [node context]
  (let [[_fn-tv fn-node fn-constrs fn-ctx] (gen/cg-node-raw (n/call-fn node) context)
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))]
    (if (contains? #{'first 'rest 'conj} fn-name)
      (handle-vector-op fn-name node context fn-node fn-constrs fn-ctx)
      (handle-ordinary-call fn-name node context fn-node fn-constrs fn-ctx))))