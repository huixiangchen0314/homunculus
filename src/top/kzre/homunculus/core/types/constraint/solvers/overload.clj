(ns top.kzre.homunculus.core.types.constraint.solvers.overload
  "COverload 约束求解器：从候选函数列表中选择匹配的重载。
   首先尝试严格统一，失败后尝试借助隐式转换匹配参数类型。
   若多个候选匹配则报告歧义，若零个则报告无匹配。"
  (:require
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn- concrete? [ty]
  (and (satisfies? tp/IType ty) (not (ty/var-type? ty))))

(defn- both-concrete? [t1 t2]
  (and (concrete? t1) (concrete? t2)))

(defn- try-convert [conversion-fn src-ty dst-ty]
  (when conversion-fn (conversion-fn src-ty dst-ty)))

(defn- relaxed-equal? [conversion-fn t1 t2]
  (or (= t1 t2)
      (and (both-concrete? t1 t2)
           (try-convert conversion-fn t1 t2))))

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

(defn- candidate-matches-args? [conversion-fn cand-args arg-tys]
  (every? (fn [[cand-arg arg-ty]]
            (relaxed-equal? conversion-fn arg-ty cand-arg))
          (map vector cand-args arg-tys)))

(defn- candidate-matches-ret? [conversion-fn cand-ret ret-tvar]
  (relaxed-equal? conversion-fn ret-tvar cand-ret))

(defn- try-match-overload-candidate
  "尝试将单个候选与期望类型匹配。
   返回 [new-subst cand] 或 nil。"
  [subst conversion-fn cand arg-tys' ret-tvar']
  (let [cand (instantiate-candidate cand subst)]
    ;; 1. 严格统一
    (try
      (let [desired' (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tvar' (reverse arg-tys'))
            new-subst (u/unify cand desired' subst)]
        [new-subst cand])
      (catch Exception _
        ;; 2. 借助隐式转换匹配
        (when conversion-fn
          (let [[cand-args cand-ret] (extract-arg-ret cand)]
            (when (= (count cand-args) (count arg-tys'))
              (when (candidate-matches-args? conversion-fn cand-args arg-tys')
                (when (candidate-matches-ret? conversion-fn cand-ret ret-tvar')
                  (let [new-subst (if (and (ty/var-type? ret-tvar')
                                           (not (contains? subst ret-tvar')))
                                    (assoc subst ret-tvar' cand-ret)
                                    subst)]
                    [new-subst cand]))))))))))

(defn solve
  "处理 COverload 约束，返回新的替换映射。"
  [overload subst conversion-fn]
  (let [arg-tys    (c/coverload-arg-tys overload)
        ret-tvar   (c/coverload-ret-tvar overload)
        fn-ty-list (c/coverload-fn-ty-list overload)
        node       (c/coverload-node overload)
        arg-tys'   (mapv #(u/substitute % subst) arg-tys)
        ret-tvar'  (u/substitute ret-tvar subst)
        successes  (keep #(try-match-overload-candidate subst conversion-fn % arg-tys' ret-tvar')
                         fn-ty-list)]
    (cond
      (empty? successes)
      (throw (ex-info "No matching overload"
                      {:arg-tys arg-tys'
                       :candidates fn-ty-list
                       :node node}))
      (= 1 (count successes))
      (first (first successes))     ;; [new-subst cand]
      :else
      (throw (ex-info "Ambiguous overload"
                      {:arg-tys arg-tys'
                       :matched (mapv second successes)
                       :node node})))))