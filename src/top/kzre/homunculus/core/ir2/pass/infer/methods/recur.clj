(ns top.kzre.homunculus.core.ir2.pass.infer.methods.recur
  (:require
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :recur [node env]
  ;; recur 不产生有意义的值，类型为 nil，此处保留 nil
  (infer/nothing node env))