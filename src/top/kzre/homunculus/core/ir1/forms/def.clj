(ns top.kzre.homunculus.core.ir1.forms.def
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'def [form env]
  (let [[_ sym & more] form
        docstring      (when (string? (first more)) (first more))
        rest-after-doc (if docstring (rest more) more)
        attr-map       (when (map? (first rest-after-doc)) (first rest-after-doc))
        val-expr       (if attr-map (second rest-after-doc) (first rest-after-doc))
        def-meta       (merge (meta form) (meta sym) (meta val-expr) attr-map)
        [val-node env'] (if val-expr
                          (ir1/lower-node val-expr env)
                          [nil env])]
    [(n/make-def sym val-node docstring def-meta) env']))