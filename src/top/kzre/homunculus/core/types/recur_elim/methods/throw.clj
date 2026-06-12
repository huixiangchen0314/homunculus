(ns top.kzre.homunculus.core.types.recur-elim.methods.throw
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :throw [node]
  (m/->ThrowNode (eliminate (:expr node))
                 (:attrs node) (:meta node) (:parent node)))