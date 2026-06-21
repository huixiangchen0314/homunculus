(ns top.kzre.homunculus.core.types.constraint.solvers.overload
  "COverload 约束求解器：从候选函数列表中选择最精确匹配的重载。
   使用 types.type 提供的工具函数简化类型操作。"
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

;; ── 利用 ty/fun-arg / ty/fun-ret 提取候选的参数类型序列 ──
(defn- fun-args [fn-ty]
  (when (ty/fun-type? fn-ty)
    (cons (ty/fun-arg fn-ty) (fun-args (ty/fun-ret fn-ty)))))

(defn- try-match-overload-candidate
  "返回 [new-subst cand-ret cost] 三元组。
   直接使用 ret-tvar' 构造期望类型，不再创建临时 TVar。"
  [subst conversion-fn cand arg-tys' ret-tvar']
  (let [cand (instantiate-candidate cand subst)]
    (try
      ;; 直接使用 ret-tvar'，构造期望的函数类型
      (let [desired (reduce (fn [ret arg] (ty/make-tfun arg ret))
                            ret-tvar'
                            (reverse arg-tys'))
            new-subst (u/unify cand desired subst)
            substituted-cand (u/substitute cand new-subst)
            real-ret (ty/fun-return-type substituted-cand)]  ;; 利用工具函数获取最终返回类型
        [new-subst real-ret 0])
      (catch Exception _
        (when conversion-fn
          (let [cand-args (fun-args cand)            ;; 获取候选参数列表
                cand-ret  (ty/fun-return-type cand)] ;; 候选最终返回类型
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

    ;; 单候选：立即严格统一
    (if (= 1 (count fn-ty-list))
      (let [cand (instantiate-candidate (first fn-ty-list) subst)
            desired (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tvar' (reverse arg-tys'))
            new-subst (try (u/unify cand desired subst)
                           (catch Exception _ subst))]
        (if (ty/var-type? ret-tvar')
          (assoc new-subst ret-tvar' (ty/fun-return-type (u/substitute cand new-subst)))
          new-subst))

      ;; 多候选：实参全部具体化后计算代价
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