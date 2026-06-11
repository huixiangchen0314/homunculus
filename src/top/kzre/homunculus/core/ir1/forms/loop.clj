;; ir1/forms/loop.clj
(ns top.kzre.homunculus.core.ir1.forms.loop
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'loop* [form]
  (let [[_ bindings & body] form
        bind-count (/ (count bindings) 2)]
    (m/->LoopNode bindings body bind-count nil [] nil)))

(defmethod ir1/build-tree :loop [node]
  (let [bindings (:bindings node)
        body     (:body node)
        bind-pairs (partition 2 bindings)
        bind-nodes (mapcat (fn [[sym val]] [(ir1/->ir1 sym) (ir1/->ir1 val)]) bind-pairs)
        body-nodes (mapv ir1/->ir1 body)
        children (into (vec bind-nodes) body-nodes)]
    (assoc node :children children)))

(defmethod ir1/form->node 'recur [form]
  (let [[_ & exprs] form]
    (m/->RecurNode (vec exprs) nil [] nil)))

(defmethod ir1/build-tree :recur [node]
  (let [expr-nodes (mapv ir1/->ir1 (:exprs node))]
    (assoc node :children expr-nodes)))