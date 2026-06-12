(ns top.kzre.homunculus.core.types.recur-elim.methods.while
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :while [node]
  (m/->WhileNode (eliminate (:test node))
                 (eliminate (:body node))
                 (:attrs node) (:meta node) (:parent node)))