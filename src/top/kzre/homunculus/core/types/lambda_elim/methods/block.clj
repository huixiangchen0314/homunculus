(ns top.kzre.homunculus.core.types.lambda-elim.methods.block
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :block [node config env]
  (let [[new-exprs defs]
        (reduce (fn [[exprs defs] expr]
                  (let [[new-expr expr-defs] (elim/eliminate expr config env)]
                    [(conj exprs new-expr) (into defs expr-defs)]))
                [[] []]
                (n/block-exprs node))]
    [(n/make-block new-exprs (n/attrs node) (n/node-meta node) (n/parent node))
     defs]))