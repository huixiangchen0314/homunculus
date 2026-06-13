(ns top.kzre.homunculus.backend.shader.methods.throw
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :throw [node backend]
  (throw (ex-info "Throw unsupported in shader" {:node node})))