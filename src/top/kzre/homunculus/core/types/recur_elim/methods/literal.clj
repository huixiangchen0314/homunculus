(ns top.kzre.homunculus.core.types.recur-elim.methods.literal
  (:require [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :literal [node]
  node)