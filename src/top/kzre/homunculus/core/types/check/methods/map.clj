(ns top.kzre.homunculus.core.types.check.methods.map
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check])
  (:import (top.kzre.homunculus.core.types.model THeteroMap)))

(defmethod check/check :map [node expected context]
  (let [kvs (n/map-kvs node)
        pairs (partition 2 kvs)]
    (if (and expected (instance? THeteroMap expected))
      (let [exp-entries (:entries expected)]
        (if (= (count pairs) (count exp-entries))
          (let [checked-pairs (mapv (fn [[k v] [k-ty v-ty]]
                                      [(check/check k k-ty context)
                                       (check/check v v-ty context)])
                                    pairs exp-entries)
                new-kvs (vec (apply concat checked-pairs))]
            (n/map-with-kvs node new-kvs))
          (throw (ex-info "Map entry count mismatch"
                          {:expected (count exp-entries) :actual (count pairs)}))))
      (let [checked-kvs (mapcat (fn [[k v]]
                                  [(check/check k nil context)
                                   (check/check v nil context)])
                                pairs)]
        (n/map-with-kvs node (vec checked-kvs))))))