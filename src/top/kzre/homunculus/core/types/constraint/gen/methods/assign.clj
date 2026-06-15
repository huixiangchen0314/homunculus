(ns top.kzre.homunculus.core.types.constraint.gen.methods.assign
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :assign [node context]
  (let [[var-tv var-node var-constr] (gen/cg-node-raw (:var node) context)
        [val-tv val-node val-constr] (gen/cg-node-raw (:val node) context)
        tv (t/->TCon :nil)
        new-node (m/->AssignNode var-node val-node (:attrs node) (:meta node) (:parent node))]
    [tv (ty/set-type! new-node tv)
     (concat var-constr val-constr (list (cm/->CEqual var-tv val-tv)))]))