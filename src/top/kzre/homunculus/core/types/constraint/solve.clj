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

(defn solve-constraints [constraints]
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
    @subst))

(defn apply-subst [node subst]
  (walk/prewalk
    (fn [n]
      (if (satisfies? ir2p/INode n)
        (let [ty (get-in n [:attrs :type])]
          (if ty
            (assoc-in n [:attrs :type] (u/substitute ty subst))
            n))
        n))
    node))

(defn process [ir2-roots env]
  (let [{:keys [roots constraints]} (gen/generate-constraints ir2-roots env)
        subst (solve-constraints constraints)]
    (mapv #(apply-subst % subst) roots)))