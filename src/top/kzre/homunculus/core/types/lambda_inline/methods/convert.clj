(ns top.kzre.homunculus.core.types.lambda-inline.methods.convert
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :convert [node config]
  (n/make-convert (inline/eliminate-inline (n/convert-expr node) config)
                  (n/convert-src-ty node)
                  (n/convert-dst-ty node)
                  (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))