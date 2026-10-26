(ns top.kzre.homunculus.core.ir2.pass.constraint.constraints.overload
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.protocol :as p]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.unify :as unify]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as env]
    [top.kzre.homunculus.core.ir2.node :as n]))

(defn- instantiate-candidate [cand subst-map]
  (let [cand (unify/substitute cand subst-map)]
    (if (scheme/tscheme? cand)
      (scheme/instantiate cand)
      cand)))

(defn- call-driven-interface? [opts]
  (:call-driven-interface? opts))

(defn- bind-fn-tv
  "若 fn-tv 是未绑定或部分绑定的类型变量，将其绑定到候选函数类型。

   判定依据是绑定是否 concrete，不是 contains? 也不是 var-type?：
   - 从未绑定            → unify 建立 fn-tv → candidate
   - 已绑定且 concrete   → 保留（已完整，不覆盖）
   - 已绑定但非 concrete → unify 合并当前绑定和 candidate，
                          保留两边已知的部分

   注意 fn-tv 可能是 substitute 后的非 TVar 形态（例如部分具体的
   TFun(int, TVar-X)），同样走 unify——保证 candidate 的约束能和
   当前形态合并，避免部分绑定时信息丢失。"
  [subst-map fn-tv candidate opts]
  (cond
    (nil? fn-tv)
    subst-map

    ;; 已完整绑定：保留（冲突检查由调用方负责，见 :solve-constraint）
    (and (not (call-driven-interface? opts))
         (ty/concrete? (unify/substitute fn-tv subst-map)))
    subst-map

    (and (call-driven-interface? opts)
         (ty/concrete? (unify/substitute fn-tv subst-map))
         (ty/concrete? (unify/substitute candidate subst-map)))
    subst-map

    ;; 其他情况（含部分具体的 TFun）统一走 unify：
    ;; unify 内部先 substitute 再按 kind 分派，能正确处理
    ;; TVar / TFun(具体,TVar) / TFun(TVar,TVar) 等形态
    :else
    (try
      (unify/unify fn-tv candidate subst-map)
      (catch Exception e
        (throw (ex-info "fn-tv binding conflict"
                        {:fn-tv     fn-tv
                         :current   (unify/substitute fn-tv subst-map)
                         :candidate candidate}
                        e))))))

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

(defrecord COverload
  [fn-type-vec arg-tys ret-tv fn-tv
   opts ]
  p/IConstraint
  (solved? [_]
   (and  (ty/concrete? ret-tv)
         (ty/concrete? fn-tv)))
  (substitute-constraint [this subst-map]
    (if (and (call-driven-interface? opts)
             (= 1 (count fn-type-vec)))
      ;; call-driven + 单候选：先把 fn-tv 绑定到候选，
      ;; 再用合并后的 subst-map 刷新所有字段
      (let [cand       (unify/substitute (first fn-type-vec) subst-map)
            subst-map' (try (unify/unify fn-tv cand subst-map)
                            (catch Exception _ subst-map))]
        (assoc this
          :fn-type-vec [cand]
          :arg-tys     (mapv #(unify/substitute % subst-map') arg-tys)
          :ret-tv      (unify/substitute ret-tv subst-map')
          :fn-tv       (unify/substitute fn-tv subst-map')))
      ;; 默认：只刷新字段，不引入新绑定
      (assoc this
        :fn-type-vec (mapv #(unify/substitute % subst-map) fn-type-vec)
        :arg-tys     (mapv #(unify/substitute % subst-map) arg-tys)
        :ret-tv      (unify/substitute ret-tv subst-map)
        :fn-tv       (unify/substitute fn-tv subst-map))))
  (solve-constraint [_ subst-map env]
    (let [arg-tys'   (mapv #(unify/substitute % subst-map) arg-tys)
          ret-tv'   (unify/substitute ret-tv subst-map)
          fn-tv'    (unify/substitute fn-tv subst-map)
          uncertain? (some #(not (ty/concrete? %)) arg-tys')
          node       (:data opts)
          callee-name (n/var-name (n/call-fn node))]
      (if (= 1 (count fn-type-vec))
        ;; ── 单候选——无重载，成功/失败都只绑一次 fn-tv ──
        (let [cand (first fn-type-vec)
              cand'      (instantiate-candidate cand subst-map)
              desired   (ty/make-fun-type arg-tys' ret-tv')
              new-subst (try (unify/unify cand' desired subst-map)
                             (catch Exception _ subst-map))
              fn-type   (unify/substitute cand' new-subst)
              new-subst (if (not (ty/concrete? ret-tv'))
                          (assoc new-subst ret-tv'
                                           (ty/fun-return-type fn-type))
                          new-subst)]
          ;(println "single solve overload: "
          ;         callee-name)
          (bind-fn-tv new-subst fn-tv' fn-type opts))

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
                               :opts opts}))
              (let [opts opts
                    grouped  (group-by :cost scored)
                    min-cost (apply min (keys grouped))
                    best     (get grouped min-cost)]
                (if (> (count best) 1)
                  (throw (ex-info "Ambiguous overload"
                                  {:arg-tys arg-tys'
                                   :opts opts
                                   :matched (map :fn-type best)}))
                  (let [{:keys [subst ret fn-type]} (first best)
                        subst (if (not (ty/concrete? ret-tv'))
                                (assoc subst ret-tv' ret)
                                subst)]
                    ;(println "solve overload: "
                    ;         (n/var-name (n/call-fn (:data opts))))
                    (bind-fn-tv subst fn-tv' fn-type opts)))))))))))

(defn make-coverload
  [fn-type-vec arg-tys ret-tv fn-tv
   & {:keys [data] :as opts}]
  (->COverload fn-type-vec arg-tys ret-tv fn-tv opts))