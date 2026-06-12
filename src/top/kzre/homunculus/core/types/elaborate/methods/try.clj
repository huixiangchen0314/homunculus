(ns top.kzre.homunculus.core.types.elaborate.methods.try
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :try [node ir2-roots config new-defs]
  (m/->TryNode (mapv #(eliminate % ir2-roots config new-defs) (:body node))
               (mapv #(eliminate % ir2-roots config new-defs) (:catches node))
               (when (:finally node)
                 (mapv #(eliminate % ir2-roots config new-defs) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))