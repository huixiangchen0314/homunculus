(ns top.kzre.homunculus.core.types.recur-elim.methods.catch
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :catch [node]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv eliminate (:body node))
                 (:attrs node) (:meta node) (:parent node)))