(ns top.kzre.homunculus.backend.shader.methods.literal
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :literal [node backend]
  (sp/shader-literal backend (:val node)))