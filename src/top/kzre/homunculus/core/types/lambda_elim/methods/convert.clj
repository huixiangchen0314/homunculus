(ns top.kzre.homunculus.core.types.lambda-elim.methods.convert
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :convert [node roots config defs]
  (n/make-convert (elim/eliminate (n/convert-expr node) roots config defs)
                  (n/convert-src-ty node)
                  (n/convert-dst-ty node)
                  (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))