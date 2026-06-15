(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。
   支持重载消解（含歧义检测）、TScheme 实例化与泛型化，以及隐式转换。
   重载匹配过程中若严格统一失败，会尝试借助隐式转换放宽参数类型差异。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.constraint.gen :as gen]
    [top.kzre.homunculus.core.types.constraint.model :as cm]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty])
  (:import
    (top.kzre.homunculus.core.types.constraint.model CConvert CEqual COverload)))

;; ── 辅助谓词 ──────────────────────────────

(defn- concrete?
  "判断类型是否为非类型变量的具体类型。"
  [ty]
  (and (satisfies? tp/IType ty)
       (not (ty/var-type? ty))))

(defn- both-concrete?
  "两个类型是否都是具体类型。"
  [t1 t2]
  (and (concrete? t1) (concrete? t2)))

;; ── 隐式转换辅助 ─────────────────────────

(defn- try-convert
  "如果存在转换函数，尝试从 src-ty 转换到 dst-ty，返回代价或 nil。"
  [conversion-fn src-ty dst-ty]
  (when conversion-fn
    (conversion-fn src-ty dst-ty)))

(defn- relaxed-equal?
  "判断两个类型是否相等，或者通过隐式转换兼容。"
  [conversion-fn t1 t2]
  (or (= t1 t2)
      (and (both-concrete? t1 t2)
           (try-convert conversion-fn t1 t2))))

;; ── 重载匹配 ─────────────────────────────

(defn- instantiate-candidate
  "对候选类型应用替换并可能实例化 scheme。"
  [cand subst]
  (let [cand (u/substitute cand subst)]
    (if (scheme/tscheme? cand)
      (scheme/instantiate cand)
      cand)))

(defn- extract-arg-ret
  "从函数类型展开参数类型列表和最终返回类型。"
  [fun-ty]
  [(take-while some? (map :arg (iterate :ret fun-ty)))
   (loop [cur fun-ty]
     (if (ty/fun-type? cur)
       (recur (:ret cur))
       cur))])

(defn- candidate-matches-args?
  "检查所有参数位置是否相等或可隐式转换。"
  [conversion-fn cand-args arg-tys]
  (every? (fn [[cand-arg arg-ty]]
            (relaxed-equal? conversion-fn arg-ty cand-arg))
          (map vector cand-args arg-tys)))

(defn- candidate-matches-ret?
  "检查返回类型是否相等或可隐式转换。"
  [conversion-fn cand-ret ret-tvar]
  (relaxed-equal? conversion-fn ret-tvar cand-ret))

(defn- try-match-overload-candidate
  "尝试将单个候选与期望类型匹配。
   subst       当前替换
   conversion-fn 隐式转换函数
   cand        候选类型
   arg-tys'    替换后的实参类型
   ret-tvar'   替换后的返回类型变量
   返回 [new-subst cand] 或 nil。"
  [subst conversion-fn cand arg-tys' ret-tvar']
  (let [cand (instantiate-candidate cand subst)]
    ;; 1. 严格统一
    (try
      (let [desired' (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tvar' (reverse arg-tys'))
            new-subst (u/unify cand desired' subst)]
        [new-subst cand])
      (catch Exception _
        ;; 2. 严格统一失败，尝试借助隐式转换放宽参数类型
        (when conversion-fn
          (let [[cand-args cand-ret] (extract-arg-ret cand)]
            (when (= (count cand-args) (count arg-tys'))
              (when (candidate-matches-args? conversion-fn cand-args arg-tys')
                (when (candidate-matches-ret? conversion-fn cand-ret ret-tvar')
                  [subst cand]  ;; 不修改替换，视为匹配成功
                  )))))))))

(defn- resolve-overload
  "处理重载约束，返回新的替换。
   若失败抛出异常，若歧义抛出异常。"
  [subst conversion-fn {:keys [fn-ty-list arg-tys ret-tvar node]}]
  (let [arg-tys' (mapv #(u/substitute % subst) arg-tys)
        ret-tvar' (u/substitute ret-tvar subst)
        successes (keep #(try-match-overload-candidate subst conversion-fn % arg-tys' ret-tvar')
                        fn-ty-list)]
    (cond
      (empty? successes)
      (throw (ex-info "No matching overload"
                      {:arg-tys arg-tys'
                       :candidates fn-ty-list
                       :node node}))

      (= 1 (count successes))
      (first (first successes))  ;; [new-subst cand]

      :else
      (throw (ex-info "Ambiguous overload"
                      {:arg-tys arg-tys'
                       :matched (mapv second successes)
                       :node node})))))

;; ── 单约束处理 ──────────────────────────

(defn- process-cequal
  "处理相等约束，返回新的替换映射。
   若统一失败但双方可隐式转换，则忽略此约束。"
  [subst conversion-fn {:keys [tvar type]}]
  (try
    (u/unify tvar type subst)
    (catch Exception e
      (let [t1 (u/substitute tvar subst)
            t2 (u/substitute type subst)]
        (if (and (both-concrete? t1 t2)
                 (try-convert conversion-fn t1 t2))
          subst   ;; 存在转换，保留原替换
          (throw e))))))

(defn- process-cconvert
  "处理显式转换约束，返回新的替换映射。
   若统一失败但双方可隐式转换，则忽略此约束。"
  [subst conversion-fn {:keys [src-ty dst-ty]}]
  (try
    (u/unify src-ty dst-ty subst)
    (catch Exception e
      (let [s (u/substitute src-ty subst)
            d (u/substitute dst-ty subst)]
        (if (and (both-concrete? s d)
                 (try-convert conversion-fn s d))
          subst
          (throw e))))))

(defn- apply-constraint
  "根据约束类型分派处理，返回更新后的替换映射。"
  [subst conversion-fn constraint]
  (cond
    (instance? CEqual constraint)
    (process-cequal subst conversion-fn constraint)

    (instance? COverload constraint)
    (resolve-overload subst conversion-fn constraint)

    (instance? CConvert constraint)
    (process-cconvert subst conversion-fn constraint)

    :else
    subst))

;; ── 主求解循环 ──────────────────────────

(defn solve-constraints
  "求解约束集合，返回替换映射。
   conversion-fn 可选，用于隐式转换。"
  ([constraints]
   (solve-constraints constraints nil))
  ([constraints conversion-fn]
   (let [subst (atom {})]
     (loop []
       (let [old-subst @subst]
         (doseq [c constraints]
           (swap! subst apply-constraint conversion-fn c))
         (when (not= old-subst @subst)
           (recur))))
     (let [final-subst @subst]
       (into {} (map (fn [[k v]] [k (u/substitute v final-subst)]) final-subst))))))

;; ── 应用替换 ─────────────────────────────

(defn apply-subst
  "将替换映射 subst 应用到节点树，更新所有类型变量。"
  [node subst]
  (walk/prewalk
    (fn [n]
      (if (satisfies? ir2p/INode n)
        (if-let [ty (ty/get-type n)]
          (if (and (ty/var-type? ty) (contains? subst ty))
            (ty/set-type! n (u/substitute ty subst))
            n)
          n)
        n))
    node))

;; ── 对外入口 ─────────────────────────────

(defn process
  "对 IR2 节点树进行约束求解。
   ir2-roots 待求解的 IR2 节点树。
   context   全局编译上下文，可包含 :frontend / :backend 以提供隐式转换支持。"
  [ir2-roots context]
  (let [context {:env       (get context :env {})
                 :frontend  (:frontend context)
                 :backend   (:backend context)}
        {:keys [roots constraints]} (gen/generate-constraints ir2-roots context)
        conversion-fn (or (when-let [be (:backend context)]
                            (fn [s d] (tp/type-conversion be s d)))
                          (when-let [fe (:frontend context)]
                            (fn [s d] (tp/type-conversion fe s d))))
        subst (solve-constraints constraints conversion-fn)]
    (mapv #(apply-subst % subst) roots)))