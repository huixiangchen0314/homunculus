;; top.kzre.homunculus.core.types.infer.methods.define.clj
(ns top.kzre.homunculus.core.types.infer.methods.define
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :define [node context]
  (let [[val-ty val-node] (infer/local-infer (:val node) context)]
    (if val-ty
      (infer/success val-ty
                     (assoc node :val val-node :attrs (assoc (:attrs node) :type val-ty)))
      (infer/nothing (assoc node :val val-node)))))