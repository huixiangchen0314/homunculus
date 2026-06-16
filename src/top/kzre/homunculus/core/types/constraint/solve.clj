(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。
   支持重载消解（含歧义检测）、TScheme 实例化与泛型化，以及隐式转换。
   求解完成后，对根节点中仍未绑定的类型变量进行泛型化（TScheme）。
   重载匹配过程中若严格统一失败，会尝试借助隐式转换放宽参数类型差异。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty])
  (:import
    (top.kzre.homunculus.core.types.constraint.model CEqual COverload CConvert)))

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
  (let [args (take-while some? (map ty/fun-arg (iterate ty/fun-ret fun-ty)))
        ret  (loop [cur fun-ty]
               (if (ty/fun-type? cur)
                 (recur (ty/fun-ret cur))
                 cur))]
    [args ret]))

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
   subst          当前替换
   conversion-fn  隐式转换函数
   cand           候选类型
   arg-tys'       替换后的实参类型
   ret-tvar'      替换后的返回类型变量
   返回 [new-subst cand] 或 nil。
   当借助隐式转换匹配时，若 ret-tvar' 仍为 TVar，则将其绑定为候选的返回类型。"
  [subst conversion-fn cand arg-tys' ret-tvar']
  (let [cand (instantiate-candidate cand subst)]
    ;; 1. 严格统一
    (try
      (let [desired' (reduce (fn [ret arg] (ty/make-tfun arg ret)) ret-tvar' (reverse arg-tys'))
            new-subst (u/unify cand desired' subst)]
        [new-subst cand])
      (catch Exception _
        ;; 2. 严格统一失败，尝试借助隐式转换放宽参数类型
        (when conversion-fn
          (let [[cand-args cand-ret] (extract-arg-ret cand)]
            (when (= (count cand-args) (count arg-tys'))
              (when (candidate-matches-args? conversion-fn cand-args arg-tys')
                (when (candidate-matches-ret? conversion-fn cand-ret ret-tvar')
                  ;; 匹配成功，若 ret-tvar' 仍是 TVar 且未绑定，则绑定为 cand-ret
                  (let [new-subst (if (and (ty/var-type? ret-tvar')
                                           (not (contains? subst ret-tvar')))
                                    (assoc subst ret-tvar' cand-ret)
                                    subst)]
                    [new-subst cand]))))))))))

(defn- resolve-overload
  "处理重载约束，返回新的替换。
   若失败抛出异常，若歧义抛出异常。"
  [subst conversion-fn overload]
  (let [arg-tys   (c/coverload-arg-tys overload)
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
      (first (first successes))  ;; [new-subst cand]

      :else
      (throw (ex-info "Ambiguous overload"
                      {:arg-tys arg-tys'
                       :matched (mapv second successes)
                       :node node})))))

;; ── 单约束处理 ──────────────────────────

(defn- process-cequal [subst conversion-fn cequal]
  (let [tvar (c/cequal-tvar cequal)
        type (c/cequal-type cequal)]
    (try
      (u/unify tvar type subst)
      (catch Exception _
        (let [t1 (u/substitute tvar subst)
              t2 (u/substitute type subst)]
          (if (and (both-concrete? t1 t2)
                   (try-convert conversion-fn t1 t2))
            subst
            subst))))))   ;; 统一失败，放弃求解，由 check-pass 处理

(defn- process-cconvert [subst conversion-fn cconvert]
  (let [src-ty (c/cconvert-src-ty cconvert)
        dst-ty (c/cconvert-dst-ty cconvert)]
    (try
      (u/unify src-ty dst-ty subst)
      (catch Exception _
        (let [s (u/substitute src-ty subst)
              d (u/substitute dst-ty subst)]
          (if (and (both-concrete? s d)
                   (try-convert conversion-fn s d))
            subst
            subst))))))   ;; 统一失败，放弃求解，由 check-pass 处理

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

;; ── 泛化未绑定的类型变量 ────────────────

(defn- generalize-node-type
  "若节点类型中存在未被替换的 TVar，将其泛化为 TScheme，否则保持原样。
   泛型环境为空（顶层），因此所有自由 TVar 都会被量化。"
  [node]
  (if-let [ty (ty/get-type node)]
    (let [ftvs (scheme/ftv ty)]
      (if (seq ftvs)
        (let [scheme (scheme/generalize ty {})]  ;; 环境为空：所有 ftv 都泛化
          (ty/set-type! node scheme))
        node))
    node))

(defn- generalize-tree
  "递归遍历节点树，对每个节点应用 generalize-node-type。"
  [root]
  (walk/prewalk
    (fn [n]
      (if (satisfies? ir2p/INode n)
        (generalize-node-type n)
        n))
    root))

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
  "对 IR2 节点树进行约束求解，随后对仍未绑定的类型变量进行泛型化。
   ir2-roots 待求解的 IR2 节点树。
   context   全局编译上下文，可包含 :frontend / :backend 以提供隐式转换支持。"
  [ir2-roots context]
  (let [context {:env       (get context :env {})
                 :frontend  (:frontend context)
                 :backend   (:backend context)}
        {:keys [roots constraints]} (gen/generate-constraints ir2-roots context)
        ;; 只从后端获取转换函数，前端根本没有 type-conversion 方法
        conversion-fn (when-let [be (:backend context)]
                        (fn [s d] (tp/type-conversion be s d)))
        subst (solve-constraints constraints conversion-fn)
        typed-roots (mapv #(apply-subst % subst) roots)]
    (mapv generalize-tree typed-roots)))