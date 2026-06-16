(ns top.kzre.homunculus.core.types.lambda-elim.methods.assign
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :assign [node roots config defs]
  (n/make-assign (elim/eliminate (n/assign-var node) roots config defs)
                 (elim/eliminate (n/assign-val node) roots config defs)
                 (n/attrs node) (n/node-meta node) (n/parent node)))