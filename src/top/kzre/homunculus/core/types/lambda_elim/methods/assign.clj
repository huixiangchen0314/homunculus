(ns top.kzre.homunculus.core.types.lambda-elim.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :assign [node config env]
  (let [[new-var var-defs] (elim/eliminate (n/assign-var node) config env)
        [new-val val-defs] (elim/eliminate (n/assign-val node) config env)]
    [(n/make-assign new-var new-val (n/attrs node) (n/node-meta node) (n/parent node))
     (into var-defs val-defs)]))