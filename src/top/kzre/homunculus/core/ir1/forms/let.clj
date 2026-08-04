(ns top.kzre.homunculus.core.ir1.forms.let
  "let* 特殊形式的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'let [form]
  (let [[_ bindings & body] form
        pairs (partition 2 bindings)
        bnds (mapv #(m/->Binding (ir1/->ir1 (first %))
                                 (ir1/->ir1 (second %))
                                 (meta bindings))
                   pairs)]
    (n/make-let bnds (n/wrap-body (mapv ir1/->ir1 body)) (meta form))))   ;; body 仍为原始表单向量
