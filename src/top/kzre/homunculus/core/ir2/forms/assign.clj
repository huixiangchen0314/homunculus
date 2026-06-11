(ns top.kzre.homunculus.core.ir2.forms.assign
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :set! [ir1-vec env]
  ;; IR1 :set! 向量: [node var val]
  (let [var-ir (second ir1-vec)
        val-ir (nth ir1-vec 2)
        var    (first (ir2/lower-ast var-ir env))
        val    (first (ir2/lower-ast val-ir env))
        meta   (ir2/ir1-meta ir1-vec)]
    [(ir2/assign-expr var val meta)]))