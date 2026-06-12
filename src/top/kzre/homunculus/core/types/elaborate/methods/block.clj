(ns top.kzre.homunculus.core.types.elaborate.methods.block
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :block [node ir2-roots config new-defs]
  (m/->BlockNode (mapv #(eliminate % ir2-roots config new-defs) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))