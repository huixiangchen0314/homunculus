;; top.kzre.homunculus.core.types.infer.methods.lambda.clj
(ns top.kzre.homunculus.core.types.infer.methods.lambda
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]))

(defmethod infer/local-infer :lambda [node context]
  ;; 局部推导不处理 lambda，返回 nothing
  (infer/nothing node context))