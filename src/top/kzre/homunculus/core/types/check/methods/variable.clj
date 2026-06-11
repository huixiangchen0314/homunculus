(ns top.kzre.homunculus.core.types.check.methods.variable
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :variable [node expected context]
  (check/check-type node expected context))