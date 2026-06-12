(ns top.kzre.homunculus.core.types.elaborate.methods.if
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :if [node ir2-roots config new-defs]
  (m/->IfNode (eliminate (:test node) ir2-roots config new-defs)
              (eliminate (:then node) ir2-roots config new-defs)
              (when (:else node) (eliminate (:else node) ir2-roots config new-defs))
              (:attrs node) (:meta node) (:parent node)))