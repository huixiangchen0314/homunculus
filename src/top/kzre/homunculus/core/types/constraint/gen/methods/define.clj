(ns top.kzre.homunculus.core.types.constraint.gen.methods.define
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :define [node context]
  (let [[val-tv val-node val-constr] (gen/cg-node-raw (:val node) context)
        new-node (m/->DefineNode (:name node) val-node (:doc node) (:attrs node) (:meta node) (:parent node))]
    [val-tv (ty/set-type! new-node val-tv) val-constr]))