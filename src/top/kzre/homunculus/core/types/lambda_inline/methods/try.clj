(ns top.kzre.homunculus.core.types.inline-lift.methods.try
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :try [node config lifted]
  (m/->TryNode (mapv #(walk % config lifted) (:body node))
               (mapv #(walk % config lifted) (:catches node))
               (when (:finally node) (mapv #(walk % config lifted) (:finally node)))
               (:attrs node) (:meta node) (:parent node)))