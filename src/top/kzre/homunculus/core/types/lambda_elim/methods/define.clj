(ns top.kzre.homunculus.core.types.lambda-elim.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :define [node config env]
  (let [[new-val val-defs] (if-let [val (n/define-val node)]
                             (elim/eliminate val config env)
                             [nil []])]
    [(n/make-define (n/define-name node) new-val (n/define-doc node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     val-defs]))