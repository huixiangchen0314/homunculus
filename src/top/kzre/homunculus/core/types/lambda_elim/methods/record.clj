(ns top.kzre.homunculus.core.types.lambda-elim.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :record [node roots config defs]
  (let [new-fields (mapv (fn [field]
                           (if-let [init (n/field-init field)]
                             (n/field-with-init field (elim/eliminate init roots config defs))
                             field))
                         (n/record-fields node))]
    (n/make-record (n/record-name node)
                   new-fields
                   (n/record-protocols node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))