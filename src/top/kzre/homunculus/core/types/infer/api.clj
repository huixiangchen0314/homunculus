(ns top.kzre.homunculus.core.types.infer.api
  (:require [top.kzre.homunculus.core.types.infer.core :as c]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.infer.methods.variable]
            [top.kzre.homunculus.core.types.infer.methods.call]
            [top.kzre.homunculus.core.types.infer.methods.if]
            [top.kzre.homunculus.core.types.infer.methods.while]
            [top.kzre.homunculus.core.types.infer.methods.block]
            [top.kzre.homunculus.core.types.infer.methods.let]
            [top.kzre.homunculus.core.types.infer.methods.lambda]
            [top.kzre.homunculus.core.types.infer.methods.loop]
            [top.kzre.homunculus.core.types.infer.methods.recur]
            [top.kzre.homunculus.core.types.infer.methods.define]
            [top.kzre.homunculus.core.types.infer.methods.vector]
            [top.kzre.homunculus.core.types.infer.methods.map]
            [top.kzre.homunculus.core.types.infer.methods.default]))

(defn make-context
  [frontend]
  {:frontend frontend})

;; re-export
(def infer c/infer)