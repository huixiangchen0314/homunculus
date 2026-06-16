(ns top.kzre.homunculus.core.types.infer.methods.protocol
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :protocol [node context]
  (infer/nothing node))