(ns top.kzre.homunculus.core.types.recur-elim.methods.block
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :block [node]
  (m/->BlockNode (mapv eliminate (:exprs node))
                 (:attrs node) (:meta node) (:parent node)))