(ns top.kzre.homunculus.core.types.constraint.constraints.project
  (:require
    [top.kzre.homunculus.core.types.constraint.protocol :as p]
    [top.kzre.homunculus.core.types.constraint.unify :as unify]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.internal.protocol :as proto]))

;; target-tv + member => ret-tv
(defrecord CProject [target-tv member ret-tv]
 p/IConstraint
  (solved? [_] (and (ty/concrete? target-tv)
                    (ty/concrete? ret-tv)))
  (substitute-constraint [this subst-map]
    (assoc this
      :target-tv (unify/substitute target-tv subst-map)
      :ret-tv   (unify/substitute ret-tv subst-map)))
 (solve-constraint [_ subst-map env]
   (let [ctx (:ctx env)
         ;; 将关键字转换为符号，以便查找字段
         member-sym (if (keyword? member) (symbol (name member)) member)
         target-ty (unify/substitute target-tv subst-map)]
     (if (ty/con-type? target-ty)
       (let [record-name (ty/type-sym target-ty)
             symbol-table (proto/symbol-table ctx)
             record-entry (sym/lookup-record symbol-table record-name)]
         (if record-entry
           (if-let [field-ty (sym/lookup-field-type record-entry member-sym)]
             (try
               (unify/unify ret-tv field-ty subst-map)
               (catch Exception _ subst-map))
             (throw (ex-info "Field not found" {:record record-name :field member-sym})))
           (throw (ex-info "Record type not found" {:record record-name}))))
       subst-map))))


(defn make-cproject
  [target-tv member ret-tv]
  (->CProject target-tv member ret-tv))