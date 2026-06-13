(ns top.kzre.homunculus.backend.shader.methods.assign
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :assign [node backend]
  (sp/shader-assign backend
                    (emit (:var node) backend)
                    (emit (:val node) backend)))