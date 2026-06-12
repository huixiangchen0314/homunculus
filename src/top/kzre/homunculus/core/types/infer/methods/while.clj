(ns top.kzre.homunculus.core.types.infer.methods.while
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :while [node context]
  ;; 简单推导：推导 test 和 body，返回 body 的类型
  (let [[test-ty test-node] (infer/local-infer (:test node) context)
        [body-ty body-node] (infer/local-infer (:body node) context)]
    (if body-ty
      (infer/success body-ty
                     (assoc node :test test-node :body body-node
                                 :attrs (assoc (:attrs node) :type body-ty)))
      (infer/nothing (assoc node :test test-node :body body-node)))))