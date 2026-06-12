(ns top.kzre.homunculus.core.types.inline-lift.methods.while
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :while [node config lifted]
  (m/->WhileNode (walk (:test node) config lifted)
                 (walk (:body node) config lifted)
                 (:attrs node) (:meta node) (:parent node)))