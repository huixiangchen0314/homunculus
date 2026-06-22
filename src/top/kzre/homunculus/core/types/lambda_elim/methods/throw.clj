(ns top.kzre.homunculus.core.types.lambda-elim.methods.throw
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :throw [node config env]
  (let [[new-expr expr-defs] (elim/eliminate (n/throw-expr node) config env)]
    [(n/make-throw new-expr (n/attrs node) (n/node-meta node) (n/parent node))
     expr-defs]))