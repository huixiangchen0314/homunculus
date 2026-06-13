(ns top.kzre.homunculus.core.types.infer.methods.define
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :define [node context]
  (let [[val-ty val-node] (infer/local-infer (:val node) context)]
    (if val-ty
      (infer/success val-ty
                     (-> node
                         (assoc :val val-node)
                         (type/set-type! val-ty)))
      (infer/nothing (assoc node :val val-node)))))