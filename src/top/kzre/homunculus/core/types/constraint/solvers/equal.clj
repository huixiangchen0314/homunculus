(ns top.kzre.homunculus.core.types.constraint.solvers.equal
  "CEqual 约束求解器：要求 tvar 与 type 相等。若直接合一失败且双方为具体类型，
   则尝试隐式转换；若转换存在则忽略该约束（保留原替换），否则也忽略（推迟到 check 阶段报错）。"
  (:require
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn- concrete? [ty]
  (and (satisfies? tp/IType ty) (not (ty/var-type? ty))))

(defn- both-concrete? [t1 t2]
  (and (concrete? t1) (concrete? t2)))

(defn- try-convert [conversion-fn src-ty dst-ty]
  (when conversion-fn (conversion-fn src-ty dst-ty)))

(defn solve
  "处理 CEqual 约束，返回新的替换映射。"
  [cequal subst conversion-fn]
  (let [tvar (c/cequal-tvar cequal)
        type (c/cequal-type cequal)]
    (try
      (u/unify tvar type subst)
      (catch Exception _
        (let [t1 (u/substitute tvar subst)
              t2 (u/substitute type subst)]
          (if (and (both-concrete? t1 t2)
                   (try-convert conversion-fn t1 t2))
            subst   ;; 存在转换，允许不一致
            subst)))))) ;; 统一失败，放弃求解，由 check-pass 处理