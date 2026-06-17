(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  "约束生成：:call 节点。当实参类型全部已知且与某个候选严格匹配时，直接确定返回类型。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defn- arity->tfun [arity]
  (reduce (fn [ret param] (ty/make-tfun (:type param) ret))
          (some-> (:ret arity) :type)
          (reverse (:params arity))))

(defmethod gen/cg-node-raw :call [node context]
  (let [[fn-tv fn-node fn-constraints] (gen/cg-node-raw (n/call-fn node) context)
        fn-name (when (= (n/kind fn-node) :variable)
                  (n/var-name fn-node))
        entry (when fn-name
                (sym/lookup-in-tables fn-name (:symbol-table context)))
        candidates (when (sym/function-symbol? entry)
                     (mapv arity->tfun (sym/list-arities entry)))
        args (n/call-args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ;; 实参列表具体意味着：空列表或每个实参都是确定类型（无 TVar）
        args-concrete? (every? ty/concrete? arg-tys)
        ;; 尝试找到参数个数和每个位置类型都严格相等的候选
        matched-cand (when args-concrete?
                       (some (fn [cand]
                               (let [cand-args (take-while some? (map :arg (iterate :ret cand)))]
                                 (when (and (= (count cand-args) (count arg-tys))
                                            (every? true? (map = cand-args arg-tys)))
                                   cand)))
                             candidates))]
    (if matched-cand
      ;; 精确匹配，直接设置返回类型，不生成额外约束
      (let [ret-ty (loop [f matched-cand]
                     (if (ty/fun-type? f)
                       (recur (:ret f))
                       f))
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs node) (n/node-meta node) (n/parent node))]
        [ret-ty (ty/set-type! new-node ret-ty) fn-constraints])
      ;; 无法直接确定，生成 COverload 约束
      (let [ret-tv (gen/fresh-tvar)
            new-node (n/make-call fn-node (vec arg-nodes)
                                  (n/attrs node) (n/node-meta node) (n/parent node))]
        (if (seq candidates)
          [ret-tv
           (ty/set-type! new-node ret-tv)
           (concat (list (c/make-coverload candidates arg-tys ret-tv new-node))
                   fn-constraints arg-constraints)]
          (let [tv (gen/fresh-tvar)]
            [tv (ty/set-type! new-node tv) fn-constraints arg-constraints]))))))