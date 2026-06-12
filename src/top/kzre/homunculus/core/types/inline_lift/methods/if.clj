(ns top.kzre.homunculus.core.types.inline-lift.methods.if
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :if [node config lifted]
  (m/->IfNode (walk (:test node) config lifted)
              (walk (:then node) config lifted)
              (when (:else node) (walk (:else node) config lifted))
              (:attrs node) (:meta node) (:parent node)))