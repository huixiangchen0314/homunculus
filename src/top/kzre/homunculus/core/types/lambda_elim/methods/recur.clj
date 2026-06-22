(ns top.kzre.homunculus.core.types.lambda-elim.methods.recur
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :recur [node config env]
  (let [[new-args defs]
        (reduce (fn [[args defs] arg]
                  (let [[new-arg arg-defs] (elim/eliminate arg config env)]
                    [(conj args new-arg) (into defs arg-defs)]))
                [[] []]
                (n/recur-args node))]
    [(n/make-recur new-args (n/attrs node) (n/node-meta node) (n/parent node))
     defs]))