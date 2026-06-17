(ns top.kzre.homunculus.core.ir1.forms.def
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'def [form]
  (let [[_ sym & more] form
        docstring? (when (string? (first more)) (first more))
        rest-after-doc (if docstring? (rest more) more)
        attr-map? (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr (if attr-map? (second rest-after-doc) (first rest-after-doc))
        def-meta (merge (meta form) (meta sym) (meta val-expr))]
    (n/make-def sym docstring? attr-map? val-expr def-meta)))

(defmethod ir1/build-tree :def [node]
  (n/make-def (ir1/->ir1 (n/def-name node))
              (when-let [d (n/def-doc node)] (ir1/->ir1 d))
              (when-let [a (n/def-attr node)] (ir1/->ir1 a))
              (when-let [v (n/def-val node)] (ir1/->ir1 v))
              (n/node-meta node)
              (n/parent node)))