(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.constraint.gen :as gen]
    [top.kzre.homunculus.core.types.constraint.model :as cm]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.model :as t])
  (:import
    (top.kzre.homunculus.core.types.constraint.model CConvert CEqual COverload)))

(defn solve-constraints
  "求解约束列表，返回一个从 TVar 到具体类型的替换映射。
   对等式约束执行统一，对重载约束进行候选消解，
   最后对替换映射进行规范化（传递性闭合）。"
  [constraints]
  (let [subst (atom {})]
    (doseq [c constraints]
      (cond
        (instance? CEqual c)
        (swap! subst #(u/unify (:tvar c) (:type c) %))

        (instance? COverload c)
        (let [{:keys [fn-ty-list arg-tys ret-tvar]} c
              desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tvar (reverse arg-tys))
              result (some (fn [cand]
                             (try (let [new-subst (u/unify cand desired @subst)]
                                    [new-subst cand])
                                  (catch Exception _ nil)))
                           fn-ty-list)]
          (if result
            (reset! subst (first result))
            (throw (ex-info "Overload resolution failed" {:arg-tys arg-tys}))))

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