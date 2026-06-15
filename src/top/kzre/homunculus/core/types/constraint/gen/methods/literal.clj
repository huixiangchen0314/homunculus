(ns top.kzre.homunculus.core.types.constraint.gen.methods.literal
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :literal [node context]
  (let [frontend (:frontend context)
        ty (when frontend (tp/literal->type frontend (:val node)))
        tv (or ty (gen/fresh-tvar))
        constraint (when ty (list (cm/->CEqual tv ty)))
        new-node (ty/set-type! node tv)]
    [tv new-node constraint]))