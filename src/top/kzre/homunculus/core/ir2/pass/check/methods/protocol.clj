(ns top.kzre.homunculus.core.ir2.pass.check.methods.protocol
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :protocol [node expected context]
  node)