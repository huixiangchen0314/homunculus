(ns top.kzre.homunculus.core.types.constraint.solve
  "求解约束集合并将类型替换应用到 IR2 树。
   支持重载消解、TScheme 实例化与泛型化。"
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

(defn solve-constraints [constraints]
  (let [subst (atom {})
        apply-all (fn []
                    (doseq [c constraints]
                      (cond
                        (instance? CEqual c)
                        (swap! subst #(u/unify (:tvar c) (:type c) %))

                        (instance? COverload c)
                        (let [{:keys [fn-ty-list arg-tys ret-tvar]} c
                              desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tvar (reverse arg-tys))
                              ;; 先替换参数类型
                              arg-tys' (mapv #(u/substitute % @subst) arg-tys)
                              ret-tvar' (u/substitute ret-tvar @subst)
                              desired' (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tvar' (reverse arg-tys'))
                              result (some (fn [cand]
                                             (let [cand (u/substitute cand @subst)
                                                   cand (if (scheme/tscheme? cand)
                                                          (scheme/instantiate cand)
                                                          cand)]
                                               (try (let [new-subst (u/unify cand desired' @subst)]
                                                      [new-subst cand])
                                                    (catch Exception _ nil))))
                                           fn-ty-list)]
                          (if result
                            (reset! subst (first result))
                            (throw (ex-info "Overload resolution failed" {:arg-tys arg-tys'}))))

                        (instance? CConvert c)
                        (swap! subst #(u/unify (:src-ty c) (:dst-ty c) %)))))
        ;; 迭代直到收敛
        loop (fn []
               (let [old @subst]
                 (apply-all)
                 (if (= old @subst)
                   @subst
                   (recur))))]
    (let [final-subst (loop)]
      (into {} (map (fn [[k v]] [k (u/substitute v final-subst)]) final-subst)))))

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

(defn process [ir2-roots env]
  (let [type-env (get env :env {})
        frontend (get env :frontend)
        context {:env type-env :frontend frontend}
        {:keys [roots constraints]} (gen/generate-constraints ir2-roots context)
        subst (solve-constraints constraints)]
    (mapv #(apply-subst % subst) roots)))