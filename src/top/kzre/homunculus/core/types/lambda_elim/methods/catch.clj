(ns top.kzre.homunculus.core.types.lambda-elim.methods.catch
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :catch [node config env]
  (let [[new-class class-defs] (elim/eliminate (n/catch-class node) config env)
        [new-sym sym-defs]     (elim/eliminate (n/catch-sym node) config env)
        [new-body body-defs]
        (reduce (fn [[exprs defs] expr]
                  (let [[new-expr expr-defs] (elim/eliminate expr config env)]
                    [(conj exprs new-expr) (into defs expr-defs)]))
                [[] []]
                (n/catch-body node))]
    [(n/make-catch new-class new-sym new-body
                   (n/attrs node) (n/node-meta node) (n/parent node))
     (into class-defs (into sym-defs body-defs))]))