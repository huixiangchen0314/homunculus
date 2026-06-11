(ns top.kzre.homunculus.core.types.check.methods.block
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod check/check :block [node expected context]
  (let [exprs (:exprs node)
        ;; 除最后一个外，其他无期望；最后一个有 expected
        butlast (butlast exprs)
        last-expr (last exprs)
        checked-butlast (mapv #(check/check % nil context) butlast)
        checked-last (check/check last-expr expected context)]
    (assoc node :exprs (conj (vec checked-butlast) checked-last))))