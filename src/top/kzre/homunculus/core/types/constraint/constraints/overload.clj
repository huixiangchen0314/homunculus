(ns top.kzre.homunculus.core.types.constraint.constraints.overload
  (:require
    [top.kzre.homunculus.core.types.constraint.protocol :as p]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as unify]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.constraint.env :as env]))

(defn- instantiate-candidate [cand subst-map]
  (let [cand (unify/substitute cand subst-map)]
    (if (scheme/tscheme? cand)
      (scheme/instantiate cand)
      cand)))


(defn- try-match-overload-candidate
  "返回 [new-subst-map cand-ret cost] 三元组。
   直接使用 ret-tv 构造期望类型，不再创建临时 TVar。"
  [subst-map cand arg-tys ret-tv env]
  (let [scheme? (scheme/tscheme? cand)
        cand (instantiate-candidate cand subst-map)]
    (try
      ;; 直接使用 ret-tv，构造期望的函数类型
      (let [desired (ty/make-fun-type arg-tys ret-tv)
            new-subst (unify/unify cand desired subst-map)
            substituted-cand (unify/substitute cand new-subst)
            real-ret (ty/fun-return-type substituted-cand)]  ;; 利用工具函数获取最终返回类型
        [new-subst real-ret 0])
      (catch Exception _
        (let [cand-params (ty/fun-params cand)            ;; 获取候选参数列表
              cand-ret  (ty/fun-return-type cand)] ;; 候选最终返回类型
          (when (= (count cand-params) (count arg-tys))
            (let [costs (map (fn [p a]
                               (cond
                                 (= p a) 0
                                 (and scheme? (ty/var-type? p)) 0
                                 :else
                                 (or (env/conversion-cost env a p)
                                     Integer/MAX_VALUE)))
                             cand-params
                             arg-tys)
                  total-cost (apply + costs)]
              (when (< total-cost Integer/MAX_VALUE)
                [subst-map cand-ret total-cost]))))))))

;; 候选函数类型签名 + 参数类型 => 函数返回值类型
(defrecord COverload [fn-type-vec arg-tys ret-tv]
  p/IConstraint
  (solved? [_]
    (ty/concrete? ret-tv))
  (substitute-constraint [this subst-map]
    (assoc this
      :fn-type-vec (mapv #(unify/substitute % subst-map) fn-type-vec)
      :arg-tys (mapv #(unify/substitute % subst-map) arg-tys)
      :ret-tv (unify/substitute ret-tv subst-map)))
  (solve-constraint [_ subst-map env]
    (let [arg-tys'   (mapv #(unify/substitute % subst-map) arg-tys)
          ret-tv'  (unify/substitute ret-tv subst-map)]
      ;; 无重载情况，退化到直接合一替换
      (if (and (not (seq? fn-type-vec))
              (= 1 (count fn-type-vec)))
        (let [cand (instantiate-candidate (first fn-type-vec) subst-map)
              desired (ty/make-fun-type arg-tys' ret-tv')
              new-subst (try (unify/unify cand desired subst-map)
                             (catch Exception _ subst-map))]
          (if (not (ty/concrete? ret-tv'))
            (assoc new-subst ret-tv' (ty/fun-return-type (unify/substitute cand new-subst)))
            new-subst))
        ;; 类型还没能完全求解出来，推迟求解
        (if (some #(not (ty/concrete? %)) arg-tys')
          subst-map
          (let [scored (keep #(try-match-overload-candidate subst-map % arg-tys' ret-tv' env)
                             fn-type-vec)]
            (if (empty? scored)
              (throw (ex-info "No matching overload"
                              {:arg-tys arg-tys' :candidates fn-type-vec }))
              (let [grouped   (group-by #(nth % 2) scored)
                    min-cost  (apply min (keys grouped))
                    best      (get grouped min-cost)]
                (if (> (count best) 1)
                  (throw (ex-info "Ambiguous overload"
                                  {:arg-tys arg-tys' :matched (map second best)}))
                  (let [[new-subst cand-ret _] (first best)]
                    (if (not (ty/concrete? ret-tv'))
                      (assoc new-subst ret-tv' cand-ret)
                      new-subst)))))))))))

(defn make-coverload
  [fn-type-vec arg-tys ret-tv]
  (->COverload fn-type-vec arg-tys ret-tv))