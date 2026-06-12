(ns top.kzre.homunculus.core.types.recur-elim.methods.assign
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :assign [node]
  (m/->AssignNode (eliminate (:var node))
                  (eliminate (:val node))
                  (:attrs node) (:meta node) (:parent node)))