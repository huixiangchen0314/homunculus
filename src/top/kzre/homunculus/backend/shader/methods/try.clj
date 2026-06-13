(ns top.kzre.homunculus.backend.shader.methods.try
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :try [node backend]
  (throw (ex-info "Try/catch unsupported in shader" {:node node})))