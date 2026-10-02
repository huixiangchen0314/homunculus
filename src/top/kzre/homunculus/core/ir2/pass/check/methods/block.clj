(ns top.kzre.homunculus.core.ir2.pass.check.methods.block
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :block [node expected env]
  (let [exprs           (n/block-exprs node)
        butlast         (butlast exprs)
        last-expr       (last exprs)
        checked-butlast (mapv #(check/check-node* % nil env) butlast)
        checked-last    (check/check-node* last-expr expected env)
        checked-exprs   (conj (vec checked-butlast) checked-last)]
    (n/block-with-exprs node checked-exprs)))