(ns top.kzre.homunculus.core.types.recur-elim.methods.if
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :if [node]
  (m/->IfNode (eliminate (:test node))
              (eliminate (:then node))
              (when (:else node) (eliminate (:else node)))
              (:attrs node) (:meta node) (:parent node)))