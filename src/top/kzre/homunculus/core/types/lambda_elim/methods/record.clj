(ns top.kzre.homunculus.core.types.lambda-elim.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :record [node config env]
  (let [[new-fields defs]
        (reduce (fn [[fields defs] field]
                  (if-let [init (n/field-init field)]
                    (let [[new-init init-defs] (elim/eliminate init config env)]
                      [(conj fields (n/field-with-init field new-init))
                       (into defs init-defs)])
                    [(conj fields field) defs]))
                [[] []]
                (n/record-fields node))]
    [(n/make-record (n/record-name node) new-fields
                    (n/record-protocols node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     defs]))