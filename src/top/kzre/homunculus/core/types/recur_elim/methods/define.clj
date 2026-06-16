(ns top.kzre.homunculus.core.types.recur-elim.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :define [node]
  (n/make-define (n/define-name node)
                 (rec/eliminate (n/define-val node))
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))