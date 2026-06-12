(ns top.kzre.homunculus.core.types.inline-lift.methods.literal
  (:require [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :literal [node config lifted]
  node)