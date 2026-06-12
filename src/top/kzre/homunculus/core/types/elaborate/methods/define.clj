(ns top.kzre.homunculus.core.types.elaborate.methods.define
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :define [node ir2-roots config new-defs]
  (m/->DefineNode (:name node)
                  (eliminate (:val node) ir2-roots config new-defs)
                  (:doc node) (:attrs node) (:meta node) (:parent node)))