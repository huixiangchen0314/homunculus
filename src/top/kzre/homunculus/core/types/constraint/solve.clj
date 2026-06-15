(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。
   支持重载消解与 TScheme 实例化。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.constraint.gen :as gen]
    [top.kzre.homunculus.core.types.constraint.model :as cm]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.types.model :as t])
  (:import
    (top.kzre.homunculus.core.types.constraint.model CConvert CEqual COverload)))

(defn- solve-overload
  "解决单个重载约束，返回新的替换或抛出异常。"
  [c subst]
  (let [{:keys [fn-ty-list arg-tys ret-tvar]} c
        ;; 确保 arg-tys 和 ret-tvar 已被替换
        arg-tys' (mapv #(u/substitute % subst) arg-tys)
        ret-tvar' (u/substitute ret-tvar subst)
        desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tvar' (reverse arg-tys'))
        candidates (map #(u/substitute % subst) fn-ty-list)
        ;; 尝试每个候选，包括 TScheme 的实例化
        result (some (fn [cand]
                       (let [cand (if (scheme/tscheme? cand)
                                    (scheme/instantiate cand)
                                    cand)]
                         (try (let [new-subst (u/unify cand desired subst)]
                                [new-subst cand])
                              (catch Exception _ nil))))
                     candidates)]
    (if result
      (first result)
      (throw (ex-info "Overload resolution failed" {:arg-tys arg-tys' :candidates candidates})))))

(defn solve-constraints
  "求解约束列表，返回一个从 TVar 到具体类型的替换映射。
   先求解所有等式约束，再处理重载约束，确保参数类型已知。"
  [constraints]
  (let [subst (atom {})
        eq-constraints (filter #(instance? CEqual %) constraints)
        other-constraints (remove #(instance? CEqual %) constraints)]
    ;; 第一阶段：求解所有等式
    (doseq [c eq-constraints]
      (swap! subst #(u/unify (:tvar c) (:type c) %)))
    ;; 第二阶段：处理重载和转换
    (doseq [c other-constraints]
      (cond
        (instance? COverload c)
        (reset! subst (solve-overload c @subst))
        (instance? CConvert c)
        (swap! subst #(u/unify (:src-ty c) (:dst-ty c) %))))
    ;; 规范化：将所有间接引用（TVar -> TVar）解析为最终类型
    (into {} (map (fn [[k v]] [k (u/substitute v @subst)]) @subst))))

(defn apply-subst
  "将类型替换应用到 IR2 节点树中，更新所有 :attrs :type 的值。"
  [node subst]
  (walk/prewalk
    (fn [n]
      (if (satisfies? ir2p/INode n)
        (let [ty (get-in n [:attrs :type])]
          (if ty
            (assoc-in n [:attrs :type] (u/substitute ty subst))
            n))
        n))
    node))

(defn process
  "完整的约束处理流程：生成约束 → 求解 → 应用替换，返回最终的 IR2 根节点序列。"
  [ir2-roots env]
  (let [{:keys [roots constraints]} (gen/generate-constraints ir2-roots env)
        subst (solve-constraints constraints)]
    (mapv #(apply-subst % subst) roots)))