(ns top.kzre.homunculus.core.types.check.methods.block
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :block [node expected context]
  (let [exprs (n/block-exprs node)
        butlast (butlast exprs)
        last-expr (last exprs)
        checked-butlast (mapv #(check/check % nil context) butlast)
        checked-last (check/check last-expr expected context)
        checked-exprs (conj (vec checked-butlast) checked-last)]
    (n/block-with-exprs node checked-exprs)))