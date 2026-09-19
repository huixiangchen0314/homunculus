(ns top.kzre.homunculus.core.ir2.pass.check.methods.literal
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :literal [node expected context]
  (check/check-type node expected context))