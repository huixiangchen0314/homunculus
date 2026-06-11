;; top.kzre.homunculus.core.types.infer.methods.lambda.clj
(ns top.kzre.homunculus.core.types.infer.methods.lambda
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :lambda [node context]
  ;; 局部推导不处理 lambda，返回 nothing
  (infer/nothing node))