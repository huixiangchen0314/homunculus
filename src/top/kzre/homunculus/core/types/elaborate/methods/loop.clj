(ns top.kzre.homunculus.core.types.elaborate.methods.loop
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :loop [node ir2-roots config new-defs]
  (m/->LoopNode (mapv (fn [[v e]] [(eliminate v ir2-roots config new-defs)
                                   (eliminate e ir2-roots config new-defs)])
                      (:bindings node))
                (eliminate (:body node) ir2-roots config new-defs)
                (:attrs node) (:meta node) (:parent node)))