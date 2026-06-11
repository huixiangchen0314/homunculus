(ns top.kzre.homunculus.core.types.check.methods.vector
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :vector [node expected context]
  (let [items (mapv #(check/check % nil context) (:items node))]
    (assoc node :items items)))