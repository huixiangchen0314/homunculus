(ns top.kzre.homunculus.core.types.lambda-inline.methods.variable
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :variable [node _config]
  node)