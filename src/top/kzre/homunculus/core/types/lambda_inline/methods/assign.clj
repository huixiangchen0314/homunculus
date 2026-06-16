(ns top.kzre.homunculus.core.types.inline-lift.methods.assign
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :assign [node config lifted]
  (m/->AssignNode (walk (:var node) config lifted)
                  (walk (:val node) config lifted)
                  (:attrs node) (:meta node) (:parent node)))