(ns top.kzre.homunculus.core.types.infer.methods.assign
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :assign [node context]
  (let [[_ new-var] (infer/local-infer (n/assign-var node) context)
        [_ new-val] (infer/local-infer (n/assign-val node) context)]
    (infer/nothing (n/make-assign new-var new-val
                                  (n/attrs node) (n/node-meta node) (n/parent node)))))