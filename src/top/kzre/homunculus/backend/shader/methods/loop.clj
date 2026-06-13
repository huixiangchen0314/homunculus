(ns top.kzre.homunculus.backend.shader.methods.loop
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :loop [node backend]
  (throw (ex-info ":loop should have been eliminated" {:node node})))