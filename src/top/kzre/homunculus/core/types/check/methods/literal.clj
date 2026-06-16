(ns top.kzre.homunculus.core.types.check.methods.literal
  (:require [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :literal [node expected context]
  (check/check-type node expected context))