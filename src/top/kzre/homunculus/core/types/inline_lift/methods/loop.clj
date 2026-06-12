(ns top.kzre.homunculus.core.types.inline-lift.methods.loop
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :loop [node config lifted]
  (m/->LoopNode (mapv (fn [[v e]] [(walk v config lifted) (walk e config lifted)]) (:bindings node))
                (walk (:body node) config lifted)
                (:attrs node) (:meta node) (:parent node)))