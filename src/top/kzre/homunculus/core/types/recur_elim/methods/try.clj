(ns top.kzre.homunculus.core.types.recur-elim.methods.try
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :try [node]
  (m/->TryNode (mapv eliminate (:body node))
               (mapv eliminate (:catches node))
               (when (:finally node) (mapv eliminate (:finally node)))
               (:attrs node) (:meta node) (:parent node)))