(ns top.kzre.homunculus.core.types.constraint.constraints.equal
  (:require
    [top.kzre.homunculus.core.types.constraint.protocol :as p]
    [top.kzre.homunculus.core.types.constraint.unify :as unify]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.constraint.env :as env]))

;; tvar => type
;; tvar 为左值，type 为右值
(defrecord CEqual [tvar expected]
 p/IConstraint
  (solved? [_] (= tvar expected))
  (substitute-constraint [this subst-map]
    (assoc this
      :tvar (unify/substitute tvar subst-map)
      :type (unify/substitute expected subst-map)))
  (solve-constraint [_ subst-map env]
    (try
      (unify/unify tvar expected subst-map)
      (catch Exception e
        (let [t1 (unify/substitute tvar subst-map)
              t2 (unify/substitute expected subst-map)]
          (if (and (ty/concrete? t1)
                   (ty/concrete? t2))
            (if-let [cost (env/conversion-cost env t1 t2)]
              (if cost
                (assoc subst-map tvar t2)
                subst-map)
              subst-map)
            subst-map))))))

(defn make-cequal
  [tvar type]
  (->CEqual tvar type))