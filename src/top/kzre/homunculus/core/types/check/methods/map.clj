(ns top.kzre.homunculus.core.types.check.methods.map
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :map [node expected context]
  (let [kvs (:kvs node)
        checked-kvs (mapv (fn [[k v]]
                            [(check/check k nil context)
                             (check/check v nil context)])
                          (partition 2 kvs))]
    (assoc node :kvs (vec (apply concat checked-kvs)))))