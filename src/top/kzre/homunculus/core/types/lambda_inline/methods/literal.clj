(ns top.kzre.homunculus.core.types.lambda-inline.methods.literal
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :literal [node _config]
  node)