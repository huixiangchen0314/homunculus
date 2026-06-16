(ns top.kzre.homunculus.core.types.lambda-elim.methods.recur
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :recur [node roots config defs]
  (n/make-recur (mapv #(elim/eliminate % roots config defs) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))