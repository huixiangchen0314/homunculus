(ns top.kzre.homunculus.core.ir2.pass.check.methods.ns
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :ns [node expected context]
  node)