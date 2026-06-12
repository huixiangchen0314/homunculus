(ns top.kzre.homunculus.core.types.inline-lift.methods.throw
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :throw [node config lifted]
  (m/->ThrowNode (walk (:expr node) config lifted)
                 (:attrs node) (:meta node) (:parent node)))