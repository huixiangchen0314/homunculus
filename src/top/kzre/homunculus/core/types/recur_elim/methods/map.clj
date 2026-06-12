(ns top.kzre.homunculus.core.types.recur-elim.methods.map
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :map [node]
  (m/->MapNode (mapv eliminate (:kvs node))
               (:attrs node) (:meta node) (:parent node)))