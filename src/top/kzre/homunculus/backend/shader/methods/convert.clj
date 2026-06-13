(ns top.kzre.homunculus.backend.shader.methods.convert
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :convert [node backend]
  (sp/shader-cast backend
                  (emit (:expr node) backend)
                  (:src-type (:attrs node))
                  (:dst-type (:attrs node))))