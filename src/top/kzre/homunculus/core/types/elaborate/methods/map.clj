(ns top.kzre.homunculus.core.types.elaborate.methods.map
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :map [node ir2-roots config new-defs]
  (m/->MapNode (mapv #(eliminate % ir2-roots config new-defs) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))