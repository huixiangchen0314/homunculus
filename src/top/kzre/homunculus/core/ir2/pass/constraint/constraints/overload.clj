(ns top.kzre.homunculus.core.ir2.pass.constraint.constraints.overload
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.protocol :as p]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.unify :as unify]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.ir2.pass.constraint.env :as env]
    [top.kzre.homunculus.core.ir2.node :as n]))

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
  "尝试单个候选。

   返回 {:subst subst-map :ret 返回类型 :fn-type 函数类型 :cost 代价}，
   或 nil（不匹配）。

   :subst 不含 fn-tv 的绑定——fn-tv 只在最终选中的候选上绑一次。
   若每个候选都 bind，catch 分支会在共享 subst-map 上写入第一个
   匹配候选的 fn-tv，后续候选的 contains? 检查挡住更新——最终选中
   的候选拿不到自己的 fn-tv。"
  [subst-map cand arg-tys ret-tv env]
  (let [scheme? (scheme/tscheme? cand)
        cand    (instantiate-candidate cand subst-map)]
    (try
      (let [desired   (ty/make-fun-type arg-tys ret-tv)
            new-subst (unify/unify cand desired subst-map)
            fn-type   (unify/substitute cand new-subst)
            real-ret  (ty/fun-return-type fn-type)]
        {:subst   new-subst
         :ret     real-ret
         :fn-type fn-type
         :cost    0})
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
                {:subst   subst-map
                 :ret     cand-ret
                 :fn-type cand
                 :cost    total-cost}))))))))

(defrecord COverload [fn-type-vec arg-tys ret-tv fn-tv data]
  p/IConstraint
  (solved? [_]
   (and  (ty/concrete? ret-tv)
         (ty/concrete? fn-tv)))
  (substitute-constraint [this subst-map]
    (assoc this
      :fn-type-vec (mapv #(unify/substitute % subst-map) fn-type-vec)
      :arg-tys     (mapv #(unify/substitute % subst-map) arg-tys)
      :ret-tv      (unify/substitute ret-tv subst-map)
      :fn-tv       (unify/substitute fn-tv subst-map)))
  (solve-constraint [_ subst-map env]
    (let [arg-tys'   (mapv #(unify/substitute % subst-map) arg-tys)
          ret-tv'   (unify/substitute ret-tv subst-map)
          fn-tv'    (unify/substitute fn-tv subst-map)
          uncertain? (some #(not (ty/concrete? %)) arg-tys')
          node       (:node data)
          callee-name (n/var-name (n/call-fn node))]
      (if (= 1 (count fn-type-vec))
        ;; ── 单候选——无重载，成功/失败都只绑一次 fn-tv ──
        (let [cand      (instantiate-candidate (first fn-type-vec) subst-map)
              desired   (ty/make-fun-type arg-tys' ret-tv')
              new-subst (try (unify/unify cand desired subst-map)
                             (catch Exception _ subst-map))
              fn-type   (unify/substitute cand new-subst)
              new-subst (if (not (ty/concrete? ret-tv'))
                          (assoc new-subst ret-tv'
                                           (ty/fun-return-type fn-type))
                          new-subst)]
          (bind-fn-tv new-subst fn-tv' fn-type))

        ;; ── 多候选——按 cost 挑选，选中后绑一次 fn-tv ──
        (if uncertain?
          subst-map
          (let [scored (keep #(try-match-overload-candidate
                                subst-map % arg-tys' ret-tv' env)
                             fn-type-vec)]
            (if (empty? scored)
              (throw (ex-info "No matching overload"
                              {:arg-tys arg-tys'
                               :candidates fn-type-vec
                               :callee callee-name
                               :data data}))
              (let [grouped  (group-by :cost scored)
                    min-cost (apply min (keys grouped))
                    best     (get grouped min-cost)]
                (if (> (count best) 1)
                  (throw (ex-info "Ambiguous overload"
                                  {:arg-tys arg-tys'
                                   :matched (map :fn-type best)}))
                  (let [{:keys [subst ret fn-type]} (first best)
                        subst (if (not (ty/concrete? ret-tv'))
                                (assoc subst ret-tv' ret)
                                subst)]
                    (bind-fn-tv subst fn-tv' fn-type)))))))))))

(defn make-coverload
  [fn-type-vec arg-tys ret-tv fn-tv & [data]]
  (->COverload fn-type-vec arg-tys ret-tv fn-tv data))