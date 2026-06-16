(ns top.kzre.homunculus.core.types.lambda-elim.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :loop [node roots config defs]
  (let [new-bindings (mapv (fn [[v e]]
                             [(elim/eliminate v roots config defs)
                              (elim/eliminate e roots config defs)])
                           (n/loop-bindings node))
        new-body     (elim/eliminate (n/loop-body node) roots config defs)]
    (n/make-loop new-bindings new-body
                 (n/attrs node) (n/node-meta node) (n/parent node))))