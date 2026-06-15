(ns top.kzre.homunculus.core.types.constraint.gen.methods.variable
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :variable [node context]
  (let [env (:env context)
        name (:name node)
        binding (or (e/lookup-env env name) (e/lookup-env env (symbol name)))
        frontend (:frontend context)]
    (if binding
      (let [ty (if (scheme/tscheme? binding)
                 (scheme/instantiate binding)
                 binding)]
        [ty (ty/set-type! node ty) nil])
      (if-let [meta-ty (and frontend (tp/meta->type frontend node))]
        [meta-ty (ty/set-type! node meta-ty) nil]
        (let [tv (gen/fresh-tvar)]
          [tv (ty/set-type! node tv) nil])))))