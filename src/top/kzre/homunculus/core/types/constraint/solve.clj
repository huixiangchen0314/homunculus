(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。
   支持重载消解（含歧义检测）、TScheme 实例化与泛型化，以及隐式转换。
   求解完成后，对根节点中仍未绑定的类型变量进行泛型化（TScheme）。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]
    ;; 三个独立求解器
    [top.kzre.homunculus.core.types.constraint.solvers.equal :as equal]
    [top.kzre.homunculus.core.types.constraint.solvers.overload :as overload]
    [top.kzre.homunculus.core.types.constraint.solvers.convert :as convert])
  (:import
    (top.kzre.homunculus.core.types.constraint.model CEqual COverload CConvert)))

;; ── 单约束处理 ──────────────────────────
(defn- apply-constraint
  "根据约束类型分派处理，返回更新后的替换映射。"
  [subst conversion-fn constraint]
  (cond
    (instance? CEqual constraint)    (equal/solve constraint subst conversion-fn)
    (instance? COverload constraint) (overload/solve constraint subst conversion-fn)
    (instance? CConvert constraint)  (convert/solve constraint subst conversion-fn)
    :else subst))

;; ── 主求解循环 ──────────────────────────
(defn solve-constraints
  ([constraints] (solve-constraints constraints nil))
  ([constraints conversion-fn]
   (let [subst (atom {})]
     ;; 阶段1：只处理 CEqual 和 CConvert，直到收敛
     (loop []
       (let [old-subst @subst]
         (doseq [c constraints]
           (when (or (instance? CEqual c) (instance? CConvert c))
             (swap! subst apply-constraint conversion-fn c)))
         (when (not= old-subst @subst)
           (recur))))
     ;; 阶段2：处理 COverload 约束
     (doseq [c constraints]
       (when (instance? COverload c)
         (swap! subst apply-constraint conversion-fn c)))
     ;; 构建最终替换
     (let [final-subst @subst]
       (into {} (map (fn [[k v]] [k (u/substitute v final-subst)]) final-subst))))))

;; ── 泛化未绑定的类型变量 ────────────────
(defn- generalize-node-type [node]
  (if-let [ty (ty/get-type node)]
    (let [ftvs (scheme/ftv ty)]
      (if (seq ftvs)
        (let [scheme (scheme/generalize ty {})]
          (ty/set-type! node scheme))
        node))
    node))

(defn- generalize-tree [root]
  (walk/prewalk
    (fn [n]
      (if (satisfies? ir2p/INode n)
        (generalize-node-type n)
        n))
    root))

;; ── 应用替换 ─────────────────────────────
(defn apply-subst [node subst]
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
  (let [{:keys [roots constraints]} (gen/generate-constraints ir2-roots context)
        conversion-fn (when-let [be (:backend context)]
                        (fn [s d] (tp/type-conversion be s d)))
        subst (solve-constraints constraints conversion-fn)
        typed-roots (mapv #(apply-subst % subst) roots)]
    (mapv generalize-tree typed-roots)))