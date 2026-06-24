(ns top.kzre.homunculus.core.types.constraint.solvers.project
  (:require [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.unify :as u]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.internal.symbol :as sym]
            [top.kzre.homunculus.internal.utils :as iu]))

(ns top.kzre.homunculus.core.types.constraint.solvers.project
  (:require [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.unify :as u]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.internal.symbol :as sym]))

(defn solve [cproject subst context]
  (let [target-tv (c/cproject-target-tv cproject)
        member    (c/cproject-member cproject)
        ;; 将关键字转换为符号，以便查找字段
        member-sym (if (keyword? member) (symbol (name member)) member)
        ret-tvar  (c/cproject-ret-tvar cproject)
        target-ty (u/substitute target-tv subst)]
    (if (ty/con-type? target-ty)
      (let [record-name (ty/type-sym target-ty)
            symbol-table (:symbol-table context)
            record-entry (sym/lookup-record symbol-table record-name)]
        (if record-entry
          (if-let [field-ty (sym/lookup-field-type record-entry member-sym)]
            (try
              (u/unify ret-tvar field-ty subst)
              (catch Exception _ subst))
            (throw (ex-info "Field not found" {:record record-name :field member-sym})))
          (throw (ex-info "Record type not found" {:record record-name}))))
      subst)))