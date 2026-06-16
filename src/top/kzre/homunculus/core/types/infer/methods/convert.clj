(ns top.kzre.homunculus.core.types.infer.methods.convert
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :convert [node context]
  (let [[_ new-expr] (infer/local-infer (n/convert-expr node) context)]
    (infer/nothing (n/make-convert new-expr
                                   (n/convert-src-ty node)
                                   (n/convert-dst-ty node)
                                   (n/convert-cost node)
                                   (n/attrs node) (n/node-meta node) (n/parent node)))))