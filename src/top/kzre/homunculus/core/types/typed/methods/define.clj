(ns top.kzre.homunculus.core.types.typed.methods.define
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :define [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [[val-ty val-node s] (infer/infer (:val node) context)
          new-node (type/set-type! (assoc node :val val-node) val-ty)]
      [val-ty new-node s])))