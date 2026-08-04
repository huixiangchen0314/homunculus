(ns top.kzre.homunculus.core.ir2.forms.loop
  (:require [top.kzre.homunculus.core.ir1.ast :as m1]   ; IR1 Binding
            [top.kzre.homunculus.core.ir2.model :as m2]   ; IR2 Binding, Loop, Recur
            [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :loop [node env]
  (let [ir1-bindings (:bindings node)       ;; IR1 Binding 向量
        ir2-bindings (mapv (fn [b]
                             (let [var-node (first (ir2/lower-ast (:var b) env))
                                   val-node (first (ir2/lower-ast (:val b) env))]
                               (m2/->Binding var-node val-node {} (:meta b))))
                           ir1-bindings)
        [ir2-body _] (ir2/lower-ast (:body node) env)]
    [(m2/->Loop ir2-bindings ir2-body {} (:meta node)) env]))

(defmethod ir2/lower-ast :recur [node env]
  (let [args (mapv #(first (ir2/lower-ast % env)) (:exprs node))]
    [(m2/->Recur args {} (:meta node))]))