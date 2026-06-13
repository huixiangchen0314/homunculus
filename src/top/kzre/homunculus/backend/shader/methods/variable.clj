(ns top.kzre.homunculus.backend.shader.methods.variable
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :variable [node backend]
  (sp/shader-var-ref backend (:name node)))