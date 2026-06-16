(ns top.kzre.homunculus.core.types.lambda-elim.methods.block
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :block [node roots config defs]
  (n/make-block (mapv #(elim/eliminate % roots config defs) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))