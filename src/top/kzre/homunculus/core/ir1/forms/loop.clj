(ns top.kzre.homunculus.core.ir1.forms.loop
  "loop 和 recur 的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── loop ─────────────────────────────────
(defmethod ir1/form->node 'loop [form]
  (let [[_ bindings & body] form
        pairs (partition 2 bindings)
        bnds (mapv #(m/->Binding (ir1/->ir1 (first %))
                                 (ir1/->ir1 (second %))
                                 (meta bindings))
                   pairs)]
    (n/make-loop bnds (n/wrap-body (mapv ir1/->ir1 body)) (meta form))))

;; ── recur ─────────────────────────────────
(defmethod ir1/form->node 'recur [form]
  (let [[_ & exprs] form]
    (n/make-recur (mapv ir1/->ir1 exprs) (meta form))))