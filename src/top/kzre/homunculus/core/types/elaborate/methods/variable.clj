(ns top.kzre.homunculus.core.types.elaborate.methods.variable
  (:require [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :variable [node ir2-roots config new-defs]
  node)