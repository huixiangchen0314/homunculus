(ns top.kzre.homunculus.core.ir1.forms.loop
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'loop* [form]
  (let [[_ bindings & body] form
        bind-count (/ (count bindings) 2)]
    (m/->LoopNode bindings body bind-count (meta form) nil)))

(defmethod ir1/build-tree :loop [node]
  (let [bindings (:bindings node)
        body     (:body node)
        bind-pairs (partition 2 bindings)
        ir-bindings (mapcat (fn [[sym val]] [(ir1/->ir1 sym) (ir1/->ir1 val)]) bind-pairs)
        ir-body (mapv ir1/->ir1 body)]
    (m/->LoopNode ir-bindings ir-body (:bindings-count node) (:meta node) (:parent node))))

(defmethod ir1/form->node 'recur [form]
  (let [[_ & exprs] form]
    (m/->RecurNode (vec exprs) (meta form) nil)))

(defmethod ir1/build-tree :recur [node]
  (m/->RecurNode (mapv ir1/->ir1 (:exprs node)) (:meta node) (:parent node)))