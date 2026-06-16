(ns top.kzre.homunculus.core.types.inline-lift.methods.block
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :block [node config lifted]
  (m/->BlockNode (mapv #(walk % config lifted) (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))