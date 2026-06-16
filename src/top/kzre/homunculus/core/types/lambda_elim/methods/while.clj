(ns top.kzre.homunculus.core.types.lambda-elim.methods.while
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :while [node roots config defs]
  (n/make-while (elim/eliminate (n/while-test node) roots config defs)
                (elim/eliminate (n/while-body node) roots config defs)
                (n/attrs node) (n/node-meta node) (n/parent node)))