(ns top.kzre.homunculus.backend.shader.methods.lambda
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]))

(defmethod emit :lambda [node backend]
  (throw (ex-info "Lambda node should not appear in final IR2" {:node node})))