(ns top.kzre.homunculus.core.ir2.pass.check.methods.record
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :record [node expected env]
  (let [fields     (n/record-fields node)
        new-fields (mapv (fn [field]
                           (if-let [init (n/field-init field)]
                             (n/field-with-init field (check/check-node* init nil env))
                             field))
                         fields)]
    (n/make-record (n/record-name node)
                   new-fields
                   (n/record-protocols node)
                   (n/attrs node)
                   (n/node-meta node))))