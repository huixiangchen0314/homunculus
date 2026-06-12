(ns top.kzre.homunculus.core.types.inline-lift.methods.catch
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :catch [node config lifted]
  (m/->CatchNode (:class node) (:sym node)
                 (mapv #(walk % config lifted) (:body node))
                 (:attrs node) (:meta node) (:parent node)))