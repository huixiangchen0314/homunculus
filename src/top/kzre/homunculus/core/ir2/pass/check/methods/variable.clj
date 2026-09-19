(ns top.kzre.homunculus.core.ir2.pass.check.methods.variable
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :variable [node expected context]
  (check/check-type node expected context))