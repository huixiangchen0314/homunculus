(ns top.kzre.homunculus.core.ir2.pass.constraint.constraints.overload
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.protocol :as p]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.unify :as unify]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.ir2.pass.constraint.env :as env]))

(defn- instantiate-candidate [cand subst-map]
  (let [cand (unify/substitute cand subst-map)]
    (if (scheme/tscheme? cand)
      (scheme/instantiate cand)
      cand)))


(defn- bind-fn-tv
  "若 fn-tv 是未绑定的类型变量，将其绑定到候选函数类型。"
  [subst-map fn-tv candidate]
  (cond
    (nil? fn-tv)                subst-map
    (not (ty/var-type? fn-tv))  subst-map
    (contains? subst-map fn-tv) subst-map
    :else                       (assoc subst-map fn-tv candidate)))

(defn- try-match-overload-candidate
  "返回 [new-subst-map cand-ret cost] 三元组。"
  [subst-map cand arg-tys ret-tv fn-tv env]
  (let [scheme? (scheme/tscheme? cand)
        cand    (instantiate-candidate cand subst-map)]
    (try
      (let [desired          (ty/make-fun-type arg-tys ret-tv)
            new-subst        (unify/unify cand desired subst-map)
            substituted-cand (unify/substitute cand new-subst)
            real-ret         (ty/fun-return-type substituted-cand)
            new-subst        (bind-fn-tv new-subst fn-tv cand)]
        [new-subst real-ret 0])
      (catch Exception _
        (let [cand-params (ty/fun-params cand)
              cand-ret  (ty/fun-return-type cand)]
          (when (= (count cand-params) (count arg-tys))
            (let [costs (map (fn [p a]
                               (cond
                                 (= p a) 0
                                 (and scheme? (ty/var-type? p)) 0
                                 :else
                                 (or (env/conversion-cost env a p)
                                     Integer/MAX_VALUE)))
                             cand-params arg-tys)
                  total-cost (apply + costs)]
              (when (< total-cost Integer/MAX_VALUE)
                [(bind-fn-tv subst-map fn-tv cand) cand-ret total-cost]))))))))

(defrecord COverload [fn-type-vec arg-tys ret-tv fn-tv]
  p/IConstraint
  (solved? [_] (ty/concrete? ret-tv))
  (substitute-constraint [this subst-map]
    (assoc this
      :fn-type-vec (mapv #(unify/substitute % subst-map) fn-type-vec)
      :arg-tys     (mapv #(unify/substitute % subst-map) arg-tys)
      :ret-tv      (unify/substitute ret-tv subst-map)
      :fn-tv       (unify/substitute fn-tv subst-map)))
  (solve-constraint [_ subst-map env]
    (let [arg-tys'  (mapv #(unify/substitute % subst-map) arg-tys)
          ret-tv'  (unify/substitute ret-tv subst-map)
          fn-tv'   (unify/substitute fn-tv subst-map)]
      (if (and (not (seq? fn-type-vec))
               (= 1 (count fn-type-vec)))
        (let [cand      (instantiate-candidate (first fn-type-vec) subst-map)
              desired   (ty/make-fun-type arg-tys' ret-tv')
              new-subst (try (unify/unify cand desired subst-map)
                             (catch Exception _ subst-map))
              new-subst (if (not (ty/concrete? ret-tv'))
                          (assoc new-subst ret-tv'
                                           (ty/fun-return-type (unify/substitute cand new-subst)))
                          new-subst)
              new-subst (bind-fn-tv new-subst fn-tv' cand)]
          new-subst)
        (if (some #(not (ty/concrete? %)) arg-tys')
          subst-map
          (let [scored (keep #(try-match-overload-candidate
                                subst-map % arg-tys' ret-tv' fn-tv' env)
                             fn-type-vec)]
            (if (empty? scored)
              (throw (ex-info "No matching overload"
                              {:arg-tys arg-tys' :candidates fn-type-vec}))
              (let [grouped  (group-by #(nth % 2) scored)
                    min-cost (apply min (keys grouped))
                    best     (get grouped min-cost)]
                (if (> (count best) 1)
                  (throw (ex-info "Ambiguous overload"
                                  {:arg-tys arg-tys' :matched (map second best)}))
                  (let [[new-subst cand-ret _] (first best)]
                    (if (not (ty/concrete? ret-tv'))
                      (assoc new-subst ret-tv' cand-ret)
                      new-subst)))))))))))

(defn make-coverload
  [fn-type-vec arg-tys ret-tv fn-tv]
  (->COverload fn-type-vec arg-tys ret-tv fn-tv))