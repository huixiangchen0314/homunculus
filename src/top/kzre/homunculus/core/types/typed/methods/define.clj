(ns top.kzre.homunculus.core.types.typed.methods.define
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :define [node context]
  (let [[val-ty val-node] (infer/infer (:val node) context)
        new-attrs (assoc (ir2p/attrs node) :type val-ty)
        new-node (assoc node :val val-node :attrs new-attrs)]
    [val-ty new-node]))