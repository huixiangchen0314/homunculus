(ns top.kzre.homunculus.core.types.recur-elim.methods.throw
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :throw [node]
  (n/make-throw (rec/eliminate (n/throw-expr node))
                (n/attrs node) (n/node-meta node) (n/parent node)))