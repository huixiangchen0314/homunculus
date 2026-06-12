(ns top.kzre.homunculus.core.types.elaborate.methods.assign
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :assign [node ir2-roots config new-defs]
  (m/->AssignNode (eliminate (:var node) ir2-roots config new-defs)
                  (eliminate (:val node) ir2-roots config new-defs)
                  (:attrs node) (:meta node) (:parent node)))