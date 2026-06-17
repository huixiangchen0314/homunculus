(ns top.kzre.homunculus.core.types.constraint.solvers.convert
  "CConvert 约束求解器：要求 src-ty 与 dst-ty 相等或通过隐式转换兼容。
   若无法直接合一且双方为具体类型且有转换，则忽略该约束。"
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
  "处理 CConvert 约束，返回新的替换映射。"
  [cconvert subst conversion-fn]
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