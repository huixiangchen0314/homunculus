(ns top.kzre.homunculus.backend.shader.methods.catch
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :catch [node backend]
  (throw (ex-info "Catch unsupported in shader" {:node node})))