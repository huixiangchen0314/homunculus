(ns top.kzre.homunculus.core.types.inline-lift.methods.let
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :let [node config lifted]
  (m/->LetNode (mapv (fn [[v e]] [(walk v config lifted) (walk e config lifted)]) (:bindings node))
               (walk (:body node) config lifted)
               (:attrs node) (:meta node) (:parent node)))