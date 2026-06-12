(ns top.kzre.homunculus.core.types.elaborate.methods.while
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :while [node ir2-roots config new-defs]
  (m/->WhileNode (eliminate (:test node) ir2-roots config new-defs)
                 (eliminate (:body node) ir2-roots config new-defs)
                 (:attrs node) (:meta node) (:parent node)))