(ns top.kzre.homunculus.core.types.check.methods.protocol
  (:require [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :protocol [node expected context]
  node)