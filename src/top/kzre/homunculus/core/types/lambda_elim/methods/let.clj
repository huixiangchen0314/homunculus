(ns top.kzre.homunculus.core.types.lambda-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :let [node roots config defs]
  (n/make-let (mapv (fn [[v e]] [(elim/eliminate v roots config defs)
                                 (elim/eliminate e roots config defs)])
                    (n/let-bindings node))
              (elim/eliminate (n/let-body node) roots config defs)
              (n/attrs node) (n/node-meta node) (n/parent node)))