;; top.kzre.homunculus.core.types.infer.methods.recur.clj
(ns top.kzre.homunculus.core.types.infer.methods.recur
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :recur [node context]
  ;; recur 不产生有意义的值，类型为 nil，此处保留 nil
  (infer/nothing node))