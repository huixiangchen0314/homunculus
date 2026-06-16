(ns top.kzre.homunculus.core.types.recur-elim.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :call [node]
  (n/make-call (rec/eliminate (n/call-fn node))
               (mapv rec/eliminate (n/call-args node))
               (n/attrs node) (n/node-meta node) (n/parent node)))