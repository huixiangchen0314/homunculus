(ns top.kzre.homunculus.core.types.recur-elim.methods.call
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :call [node]
  (m/->CallNode (eliminate (:fn node))
                (mapv eliminate (:args node))
                (:attrs node) (:meta node) (:parent node)))