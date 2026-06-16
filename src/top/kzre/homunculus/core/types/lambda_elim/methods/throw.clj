(ns top.kzre.homunculus.core.types.lambda-elim.methods.throw
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :throw [node roots config defs]
  (n/make-throw (elim/eliminate (n/throw-expr node) roots config defs)
                (n/attrs node) (n/node-meta node) (n/parent node)))