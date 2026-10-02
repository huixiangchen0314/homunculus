(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.literal
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :literal [current-node env]
  (let [literal-type (p/literal-type env (n/lit-val current-node))
        ;; TODO 理论上，前端应当覆盖所有字面量的类型才行
        tv           (or literal-type (gen/fresh-tvar))
        constraints  (when literal-type
                       [(cons/make-cequal tv literal-type)])
        new-node     (t/set-type! current-node tv)]
    {:type        tv
     :node        new-node
     :constraints constraints
     :env         env}))