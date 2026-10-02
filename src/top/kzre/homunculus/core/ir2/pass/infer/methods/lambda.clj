(ns top.kzre.homunculus.core.ir2.pass.infer.methods.lambda
  (:require
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

;; 注意：全局定义的 lambda 不会进入这个分支，这里只会接受到闭包
(defmethod infer/infer-node* :lambda [node env]
  ;; 局部推导不处理闭包，返回 nothing
  (infer/nothing node env))