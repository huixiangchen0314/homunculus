(ns top.kzre.homunculus.backend.shader.methods.recur
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :recur [node backend]
  (throw (ex-info ":recur should have been eliminated" {:node node})))