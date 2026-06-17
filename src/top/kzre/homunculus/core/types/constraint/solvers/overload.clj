(ns top.kzre.homunculus.core.types.constraint.solvers.overload
  "COverload 约束求解器：从候选函数列表中选择最精确匹配的重载。
   **修复**：不再在匹配过程中创建临时 TVar，避免替换映射无限膨胀。"
  (:require
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn- concrete? [ty]
  (and (satisfies? tp/IType ty) (not (ty/var-type? ty))))

(defn- try-convert [conversion-fn src-ty dst-ty]
  (when conversion-fn (conversion-fn src-ty dst-ty)))

(defn- instantiate-candidate [cand subst]
  (let [cand (u/substitute cand subst)]
    (if (scheme/tscheme? cand)
      (scheme/instantiate cand)
      cand)))

(defn- extract-arg-ret [fun-ty]
  (let [args (take-while some? (map ty/fun-arg (iterate ty/fun-ret fun-ty)))
        ret  (loop [cur fun-ty]
               (if (ty/fun-type? cur)
                 (recur (ty/fun-ret cur))
                 cur))]
    [args ret]))

(defn- try-match-overload-candidate
  "返回 [new-subst cand cost] 三元组。
   不再创建临时 TVar，直接使用传入的 ret-tvar' 构造期望类型。"
  [subst conversion-fn cand arg-tys' ret-tvar']
  (let [cand (instantiate-candidate cand subst)]
    (try
      ;; ★ 直接使用 ret-tvar'，不创建新 TVar
      (let [desired (reduce (fn [ret arg] (ty/make-tfun arg ret))
                            ret-tvar'
                            (reverse arg-tys'))
            new-subst (u/unify cand desired subst)
            substituted-cand (u/substitute cand new-subst)
            [_ real-ret] (extract-arg-ret substituted-cand)]
        [new-subst real-ret 0])
      (catch Exception _
        (when conversion-fn
          (let [[cand-args cand-ret] (extract-arg-ret cand)]
            (when (= (count cand-args) (count arg-tys'))
              (let [costs (map (fn [carg a]
                                 (if (ty/var-type? a)
                                   0
                                   (if (= carg a)
                                     0
                                     (or (try-convert conversion-fn a carg)
                                         (try-convert conversion-fn carg a)
                                         Integer/MAX_VALUE))))
                               cand-args arg-tys')
                    total-cost (apply + costs)]
                (when (< total-cost Integer/MAX_VALUE)
                  [subst cand-ret total-cost])))))))))

(defn solve [overload subst conversion-fn]
  (let [arg-tys    (c/coverload-arg-tys overload)
        ret-tvar   (c/coverload-ret-tvar overload)
        fn-ty-list (c/coverload-fn-ty-list overload)
        node       (c/coverload-node overload)
        arg-tys'   (mapv #(u/substitute % subst) arg-tys)
        ret-tvar'  (u/substitute ret-tvar subst)]

    ;; 单候选：立即严格统一，无论实参中是否有 TVar
    (if (= 1 (count fn-ty-list))
      (let [cand (instantiate-candidate (first fn-ty-list) subst)
            ;; ★ 直接使用 ret-tvar'
            desired (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tvar' (reverse arg-tys'))
            new-subst (try (u/unify cand desired subst)
                           (catch Exception _ subst))]
        (if (ty/var-type? ret-tvar')
          (assoc new-subst ret-tvar' (second (extract-arg-ret (u/substitute cand new-subst))))
          new-subst))

      ;; 多候选：实参全部具体化后计算代价，选最优解
      (if (some ty/var-type? arg-tys')
        subst   ;; 推迟
        (let [scored (keep #(try-match-overload-candidate subst conversion-fn % arg-tys' ret-tvar')
                           fn-ty-list)]
          (if (empty? scored)
            (throw (ex-info "No matching overload"
                            {:arg-tys arg-tys' :candidates fn-ty-list :node node}))
            (let [min-cost (apply min (map #(nth % 2) scored))
                  best     (filter #(= (nth % 2) min-cost) scored)]
              (if (> (count best) 1)
                (throw (ex-info "Ambiguous overload"
                                {:arg-tys arg-tys' :matched (map second best) :node node}))
                (let [[new-subst cand-ret _] (first best)]
                  (if (ty/var-type? ret-tvar')
                    (assoc new-subst ret-tvar' cand-ret)
                    new-subst))))))))))