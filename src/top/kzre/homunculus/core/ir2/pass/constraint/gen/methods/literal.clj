(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.literal
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
   [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
   [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
   [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
   [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/cg-node-raw :literal [node context]
  (let [frontend     (u/frontend context)
        literal-type (when frontend (tp/literal->type frontend (n/lit-val node)))
        tv           (or literal-type (gen/fresh-tvar))     ;; TODO 理论上，前端应当覆盖所有字面量的类型才行.
        constraint   (when literal-type (list (cons/make-cequal tv literal-type)))
        new-node     (t/set-type! node tv)]
    [tv new-node constraint context]))