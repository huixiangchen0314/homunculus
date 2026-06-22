(ns top.kzre.homunculus.core.types.lambda-elim.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :map [node config env]
  (let [[new-kvs defs]
        (reduce (fn [[kvs defs] kv]
                  (let [[new-kv kv-defs] (elim/eliminate kv config env)]
                    [(conj kvs new-kv) (into defs kv-defs)]))
                [[] []]
                (n/map-kvs node))]
    [(n/make-map new-kvs (n/attrs node) (n/node-meta node) (n/parent node))
     defs]))