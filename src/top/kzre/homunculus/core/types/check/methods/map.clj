(ns top.kzre.homunculus.core.types.check.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :map [node expected context]
  (let [kvs (n/map-kvs node)
        pairs (partition 2 kvs)]
    (if (and expected (ty/hetero-map? expected))
      (let [exp-entries (ty/hetero-map-entries expected)]
        (if (= (count pairs) (count exp-entries))
          (let [checked-pairs (mapv (fn [[k v] [k-ty v-ty]]
                                      (n/make-pair (check/check-node k k-ty context)
                                                   (check/check-node v v-ty context)))
                                    pairs exp-entries)
                new-kvs (vec (apply concat checked-pairs))]
            (n/make-map new-kvs (n/attrs node) (n/node-meta node) (n/parent node)))
          (throw (ex-info "Map entry count mismatch"
                          {:expected (count exp-entries) :actual (count pairs)}))))
      (let [checked-kvs (mapcat (fn [[k v]]
                                  (n/make-pair (check/check-node k nil context)
                                               (check/check-node v nil context)))
                                pairs)]
        (n/make-map (vec checked-kvs) (n/attrs node) (n/node-meta node) (n/parent node))))))