(ns top.kzre.homunculus.core.types.lambda-elim.methods.convert
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :convert [node config env]
  (let [[new-expr expr-defs] (elim/eliminate (n/convert-expr node) config env)]
    [(n/make-convert new-expr
                     (n/convert-src-ty node)
                     (n/convert-dst-ty node)
                     (n/convert-cost node)
                     (n/attrs node) (n/node-meta node) (n/parent node))
     expr-defs]))