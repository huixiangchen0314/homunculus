(ns top.kzre.homunculus.core.types.recur-elim.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :assign [node]
  (n/make-assign (rec/eliminate (n/assign-var node))
                 (rec/eliminate (n/assign-val node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))