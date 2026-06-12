(ns top.kzre.homunculus.core.types.inline-lift.methods.variable
  (:require [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :variable [node config lifted]
  node)