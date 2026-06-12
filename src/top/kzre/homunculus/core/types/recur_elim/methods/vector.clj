(ns top.kzre.homunculus.core.types.recur-elim.methods.vector
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :vector [node]
  (m/->VectorNode (mapv eliminate (:items node))
                  (:attrs node) (:meta node) (:parent node)))