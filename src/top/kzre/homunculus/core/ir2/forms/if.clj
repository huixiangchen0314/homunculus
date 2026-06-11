(ns top.kzre.homunculus.core.ir2.forms.if
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :if [ir1-vec env]
  ;; IR1 :if 向量: [node test then else?]
  (let [test-ir (second ir1-vec)
        then-ir (nth ir1-vec 2)
        else-ir (nth ir1-vec 3 nil)        ; 可能不存在
        test    (first (ir2/lower-ast test-ir env))
        then    (first (ir2/lower-ast then-ir env))
        else    (when else-ir (first (ir2/lower-ast else-ir env)))
        meta    (ir2/ir1-meta ir1-vec)]
    [(ir2/if-expr test then else meta)]))