(ns top.kzre.homunculus.core.types.recur-elim.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :record [node]
  (n/make-record (n/record-name node)
                 (mapv (fn [field]
                         (if-let [init (n/field-init field)]
                           (n/field-with-init field (rec/eliminate init))
                           field))
                       (n/record-fields node))
                 (n/record-protocols node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))