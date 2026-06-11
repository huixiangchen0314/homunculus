(ns top.kzre.homunculus.core.ir2.forms.throw
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :throw [ir1-vec env]
  ;; IR1 :throw 向量: [node expr]
  (let [expr-ir (second ir1-vec)
        expr    (first (ir2/lower-ast expr-ir env))
        meta    (ir2/ir1-meta ir1-vec)]
    [(ir2/throw-expr expr meta)]))