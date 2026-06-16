(ns top.kzre.homunculus.core.types.infer.methods.record
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :record [node context]
  (let [fields     (n/record-fields node)
        new-fields (mapv (fn [field]
                           (if-let [init (n/field-init field)]
                             (let [[_ new-init] (infer/local-infer init context)]
                               (n/field-with-init field new-init))
                             field))
                         fields)
        new-node   (n/record-with-fields node new-fields)]
    (infer/nothing new-node)))