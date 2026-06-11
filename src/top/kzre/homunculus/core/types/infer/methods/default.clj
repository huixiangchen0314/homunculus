;; top.kzre.homunculus.core.types.infer.methods.default.clj
(ns top.kzre.homunculus.core.types.infer.methods.default
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]))

(defmethod infer/local-infer :default [node context]
  (infer/nothing node))