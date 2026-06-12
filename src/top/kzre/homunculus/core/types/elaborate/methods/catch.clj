(ns top.kzre.homunculus.core.types.elaborate.methods.catch
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :catch [node ir2-roots config new-defs]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(eliminate % ir2-roots config new-defs) (:body node))
                 (:attrs node) (:meta node) (:parent node)))