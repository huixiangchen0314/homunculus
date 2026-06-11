(ns top.kzre.homunculus.core.ir2.forms.let
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :let* [ir1-vec env]
  ;; IR1 :let* 向量: [node sym1 val1 sym2 val2 ... body...]
  (let [node       (first ir1-vec)
        bind-count (:bindings-count node)
        body-start (inc (* 2 bind-count))            ; 跳过节点和绑定对
        bind-irs   (take (* 2 bind-count) (rest ir1-vec))
        body-irs   (drop body-start ir1-vec)
        bindings   (mapv (fn [[sym-ir val-ir]]
                           [(first (ir2/lower-ast sym-ir env))
                            (first (ir2/lower-ast val-ir env))])
                         (partition 2 bind-irs))
        body       (if (= (count body-irs) 1)
                     (first (ir2/lower-ast (first body-irs) env))
                     (ir2/block-expr (mapv #(first (ir2/lower-ast % env)) body-irs) nil))
        meta       (ir2/ir1-meta ir1-vec)]
    [(ir2/let-expr bindings body meta)]))