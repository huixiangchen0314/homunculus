(ns top.kzre.homunculus.backend.shader.methods.map
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :map [node backend]
  (throw (ex-info "Map literals unsupported in shader" {:node node})))