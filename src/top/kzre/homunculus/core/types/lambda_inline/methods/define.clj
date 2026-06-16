(ns top.kzre.homunculus.core.types.inline-lift.methods.define
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :define [node config lifted]
  (m/->DefineNode (:name node)
                  (walk (:val node) config lifted)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))