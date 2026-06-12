(ns top.kzre.homunculus.core.types.recur-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :let [node]
  (m/->LetNode (mapv (fn [[v e]] [(eliminate v) (eliminate e)]) (:bindings node))
               (eliminate (:body node))
               (:attrs node) (:meta node) (:parent node)))