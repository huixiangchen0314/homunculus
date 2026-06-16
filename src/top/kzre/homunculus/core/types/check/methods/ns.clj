(ns top.kzre.homunculus.core.types.check.methods.ns
  (:require [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :ns [node expected context]
  node)