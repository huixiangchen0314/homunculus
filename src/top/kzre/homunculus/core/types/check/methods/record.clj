(ns top.kzre.homunculus.core.types.check.methods.record
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod check/check-node :record [node expected context]
  (let [fields     (n/record-fields node)
        new-fields (mapv (fn [field]
                           (if-let [init (n/field-init field)]
                             (n/field-with-init field (check/check-node init nil context))
                             field))
                         fields)]
    (n/make-record (n/record-name node)
                   new-fields
                   (n/record-protocols node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))