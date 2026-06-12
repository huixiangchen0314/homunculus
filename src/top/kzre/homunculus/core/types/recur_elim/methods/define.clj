(ns top.kzre.homunculus.core.types.recur-elim.methods.define
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :define [node]
  (m/->DefineNode (:name node)
                  (eliminate (:val node))
                  (:doc node) (:attrs node) (:meta node) (:parent node)))