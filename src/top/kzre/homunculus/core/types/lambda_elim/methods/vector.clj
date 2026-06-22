(ns top.kzre.homunculus.core.types.lambda-elim.methods.vector
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :vector [node config env]
  (let [[new-items defs]
        (reduce (fn [[items defs] item]
                  (let [[new-item item-defs] (elim/eliminate item config env)]
                    [(conj items new-item) (into defs item-defs)]))
                [[] []]
                (n/vector-items node))]
    [(n/make-vector new-items (n/attrs node) (n/node-meta node) (n/parent node))
     defs]))