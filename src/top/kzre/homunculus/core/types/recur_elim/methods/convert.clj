(ns top.kzre.homunculus.core.types.recur-elim.methods.convert
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :convert [node]
  (n/make-convert (rec/eliminate (n/convert-expr node))
                  (n/convert-src-ty node)
                  (n/convert-dst-ty node)
                  (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))