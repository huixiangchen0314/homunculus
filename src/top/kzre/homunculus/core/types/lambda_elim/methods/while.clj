(ns top.kzre.homunculus.core.types.lambda-elim.methods.while
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :while [node config env]
  (let [[new-test test-defs] (elim/eliminate (n/while-test node) config env)
        [new-body body-defs] (elim/eliminate (n/while-body node) config env)]
    [(n/make-while new-test new-body (n/attrs node) (n/node-meta node) (n/parent node))
     (into test-defs body-defs)]))