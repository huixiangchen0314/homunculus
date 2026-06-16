(ns top.kzre.homunculus.core.types.constraint.gen.methods.define
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :define [node context]
  (let [[val-tv val-node val-constr] (gen/cg-node-raw (n/define-val node) context)
        new-node (n/make-define (n/define-name node)
                                val-node
                                (n/define-doc node)
                                (n/attrs node)
                                (n/node-meta node)
                                (n/parent node))]
    [val-tv (ty/set-type! new-node val-tv) val-constr]))