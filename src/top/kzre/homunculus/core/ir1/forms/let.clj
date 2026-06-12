(ns top.kzre.homunculus.core.ir1.forms.let
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'let* [form]
  (let [[_ bindings & body] form
        bind-count (/ (count bindings) 2)]
    (m/->LetNode bindings body bind-count nil nil)))

(defmethod ir1/build-tree :let [node]
  (let [bindings (:bindings node)
        body     (:body node)
        bind-pairs (partition 2 bindings)
        ir-bindings (mapcat (fn [[sym val]] [(ir1/->ir1 sym) (ir1/->ir1 val)]) bind-pairs)
        ir-body (mapv ir1/->ir1 body)]
    (m/->LetNode ir-bindings ir-body (:bindings-count node) (:meta node) (:parent node))))