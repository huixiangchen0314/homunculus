(ns top.kzre.homunculus.core.types.infer.methods.while
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :while [node context]
  (let [[test-ty test-node] (infer/local-infer (:test node) context)
        [body-ty body-node] (infer/local-infer (:body node) context)]
    (if body-ty
      (infer/success body-ty
                     (type/set-type! (assoc node :test test-node :body body-node) body-ty))
      (infer/nothing (assoc node :test test-node :body body-node)))))