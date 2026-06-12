(ns top.kzre.homunculus.core.types.inline-lift.methods.map
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :map [node config lifted]
  (m/->MapNode (mapv #(walk % config lifted) (:kvs node))
               (:attrs node) (:meta node) (:parent node)))