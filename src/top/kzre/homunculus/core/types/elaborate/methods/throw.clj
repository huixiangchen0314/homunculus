(ns top.kzre.homunculus.core.types.elaborate.methods.throw
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :throw [node ir2-roots config new-defs]
  (m/->ThrowNode (eliminate (:expr node) ir2-roots config new-defs)
                 (:attrs node) (:meta node) (:parent node)))