(ns top.kzre.homunculus.core.types.constraint.gen.methods.literal
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :literal [node context]
  (let [frontend (:frontend context)
        literal-type (when frontend (tp/literal->type frontend (n/lit-val node)))
        tv (or literal-type (gen/fresh-tvar))
        constraint (when literal-type (list (c/make-cequal tv literal-type)))
        new-node (ty/set-type! node tv)]
    [tv new-node constraint]))