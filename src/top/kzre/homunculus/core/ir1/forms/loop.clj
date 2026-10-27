(ns top.kzre.homunculus.core.ir1.forms.loop
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'loop [form env]
  (let [[_ bindings & body] form
        env-outer env
        pairs     (partition 2 bindings)
        [bnds env-bind]
        (reduce (fn [[acc e] [lhs rhs]]
                  (let [lhs-node   (n/make-symbol lhs (meta lhs))
                        e'         (ir1/bind-local-symbol e lhs)
                        [rhs' e''] (ir1/lower-node rhs e')]
                    [(conj acc (m/->Binding lhs-node rhs' (meta bindings))) e'']))
                [[] env]
                pairs)
        [body-nodes _] (ir1/lower-seq body env-bind)]
    [(n/make-loop bnds (n/wrap-body body-nodes) (meta form)) env-outer]))

(defmethod ir1/lower-node* 'recur [form env]
  (let [[_ & exprs] form
        [nodes env'] (ir1/lower-seq exprs env)]
    [(n/make-recur nodes (meta form)) env']))