(ns top.kzre.homunculus.core.ir2.forms.block
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :do [ir1-vec env]
  ;; IR1 :do 向量: [node expr1 expr2 ...]
  (let [exprs (mapv #(first (ir2/lower-ast % env)) (rest ir1-vec))
        meta  (ir2/ir1-meta ir1-vec)]
    [(ir2/block-expr exprs meta)]))