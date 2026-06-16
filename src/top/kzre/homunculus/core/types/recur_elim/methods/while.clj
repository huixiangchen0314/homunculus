(ns top.kzre.homunculus.core.types.recur-elim.methods.while
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :while [node]
  (n/make-while (rec/eliminate (n/while-test node))
                (rec/eliminate (n/while-body node))
                (n/attrs node) (n/node-meta node) (n/parent node)))