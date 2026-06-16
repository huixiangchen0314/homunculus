(ns top.kzre.homunculus.core.types.infer.methods.member-access
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :member-access [node context]
  (let [[_ new-target] (infer/local-infer (n/access-target node) context)
        new-args       (mapv #(second (infer/local-infer % context)) (n/access-args node))]
    (infer/nothing (n/make-member-access new-target
                                         (n/access-member node)
                                         new-args
                                         (n/node-meta node) (n/parent node)))))