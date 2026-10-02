(ns top.kzre.homunculus.core.ir2.pass.constraint.constraints.project
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.protocol :as p]
    [top.kzre.homunculus.core.ir2.pass.constraint.unify :as unify]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.ir2.pass.constraint.env :as env]))

;; target-tv + member => ret-tv
(defrecord CProject [target-tv member ret-tv]
  p/IConstraint
  (solved? [_] (and (ty/concrete? target-tv)
                    (ty/concrete? ret-tv)))
  (substitute-constraint [this subst-map]
    (assoc this
      :target-tv (unify/substitute target-tv subst-map)
      :ret-tv    (unify/substitute ret-tv subst-map)))
  (solve-constraint [_ subst-map env]
    (let [target-type (unify/substitute target-tv subst-map)]
      (if (ty/con-type? target-type)
        (let [record-name (ty/type-sym target-type)]
          (if-let [field-type (env/resolve-field-type env record-name member)]
            (try
              (unify/unify ret-tv field-type subst-map)
              (catch Exception _ subst-map))
            (throw (ex-info "Record or field not found"
                            {:record record-name
                             :field  member}))))
        subst-map))))

(defn make-cproject
  [target-tv member ret-tv]
  (->CProject target-tv member ret-tv))