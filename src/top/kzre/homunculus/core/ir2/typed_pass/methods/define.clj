(ns top.kzre.homunculus.core.ir2.typed-pass.methods.define
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]))

(defmethod infer/infer :define [node env]
  (let [[val-ty val-node] (infer/infer (:val node) env)
        new-node (assoc node :val val-node :attrs (assoc (:attrs node) :type val-ty))]
    [val-ty new-node]))