(ns top.kzre.homunculus.core.types.elaborate.methods.literal
  (:require [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :literal [node ir2-roots config new-defs]
  node)