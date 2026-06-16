(ns top.kzre.homunculus.core.types.lambda-inline.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :record [node config]
  (let [new-fields (mapv (fn [field]
                           (if-let [init (n/field-init field)]
                             (n/field-with-init field (inline/eliminate-inline init config))
                             field))
                         (n/record-fields node))]
    (n/make-record (n/record-name node)
                   new-fields
                   (n/record-protocols node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))