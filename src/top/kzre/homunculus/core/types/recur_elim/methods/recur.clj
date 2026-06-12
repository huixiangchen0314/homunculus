(ns top.kzre.homunculus.core.types.recur-elim.methods.recur
  (:require [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :recur [node]
  node)