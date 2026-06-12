(ns top.kzre.homunculus.core.types.recur-elim.methods.loop
  (:require [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :loop [node]
  (eliminate (transform-loop node)))