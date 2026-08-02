(ns top.kzre.homunculus.core.ir2.forms.pair
  (:require
    [top.kzre.homunculus.core.ir2.core :as ir2]
    [top.kzre.homunculus.core.ir2.model :as model]))


(defmethod ir2/lower-ast :pair [node env]
  (let [[key-node env1] (ir2/lower-ast (:key node) env)
        [val-node env2] (ir2/lower-ast (:val node) env1)]
    [(model/->Pair key-node val-node {} (:meta node)) env2]))