;; ir1/forms/def.clj
(ns top.kzre.homunculus.core.ir1.forms.def
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'def [form]
  (let [[_ sym & more] form
        docstring? (when (string? (first more)) (first more))
        rest-after-doc (if docstring? (rest more) more)
        attr-map? (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr (if attr-map? (second rest-after-doc) (first rest-after-doc))]
    (m/->DefNode sym docstring? attr-map? val-expr nil [] nil)))

(defmethod ir1/build-tree :def [node]
  (let [name-ir (ir1/->ir1 (:name node))
        doc-ir  (when-let [d (:doc node)] (ir1/->ir1 d))
        attr-ir (when-let [a (:attr node)] (ir1/->ir1 a))
        val-ir  (when-let [v (:val node)] (ir1/->ir1 v))
        children (vec (remove nil? (list name-ir doc-ir attr-ir val-ir)))]
    (assoc node :children children)))