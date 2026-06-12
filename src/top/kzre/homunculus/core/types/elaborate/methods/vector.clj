(ns top.kzre.homunculus.core.types.elaborate.methods.vector
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :vector [node ir2-roots config new-defs]
  (m/->VectorNode (mapv #(eliminate % ir2-roots config new-defs) (:items node))
                  (:attrs node) (:meta node) (:parent node)))