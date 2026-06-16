(ns top.kzre.homunculus.core.types.lambda-elim.methods.member-access
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :member-access [node roots config defs]
  (n/make-member-access (elim/eliminate (n/access-target node) roots config defs)
                        (n/access-member node)
                        (mapv #(elim/eliminate % roots config defs) (n/access-args node))
                        (n/node-meta node) (n/parent node)))