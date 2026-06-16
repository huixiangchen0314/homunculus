(ns top.kzre.homunculus.core.types.recur-elim.methods.block
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :block [node]
  (n/make-block (mapv rec/eliminate (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))