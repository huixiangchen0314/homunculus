(ns top.kzre.homunculus.core.types.inline-lift.methods.vector
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :vector [node config lifted]
  (m/->VectorNode (mapv #(walk % config lifted) (:items node))
                  (:attrs node) (:meta node) (:parent node)))