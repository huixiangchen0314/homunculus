(ns top.kzre.homunculus.core.ir2.forms.loop
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :loop* [ir1-vec env]
  (let [node       (first ir1-vec)
        bind-count (:bindings-count node)
        body-start (inc (* 2 bind-count))
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
    [(ir2/loop-expr bindings body meta)]))

(defmethod ir2/lower-ast :recur [ir1-vec env]
  (let [args (mapv #(first (ir2/lower-ast % env)) (rest ir1-vec))
        meta (ir2/ir1-meta ir1-vec)]
    [(ir2/recur-expr args meta)]))