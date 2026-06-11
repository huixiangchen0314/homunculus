(ns top.kzre.homunculus.core.ir2.forms.lambda
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir2/lower-ast :fn* [ir1-vec env]
  (let [node       (first ir1-vec)
        name       (:name node)
        params     (:params node)   ;; [{:sym s :meta m} ...]
        ;; 为每个参数构造 IR1 符号节点，然后 lowering 得到 :variable
        param-irs  (mapv (fn [p] [(ir1/make-node :symbol :name (:sym p) :meta (:meta p))]) params)
        param-nodes (mapv #(first (ir2/lower-ast % env)) param-irs)
        body-start (if name 2 1)
        body-irs   (drop body-start (rest ir1-vec))
        body       (if (= (count body-irs) 1)
                     (first (ir2/lower-ast (first body-irs) env))
                     (ir2/block-expr (mapv #(first (ir2/lower-ast % env)) body-irs) nil))
        captures   []
        meta       (ir2/ir1-meta ir1-vec)]
    [(ir2/lambda-expr param-nodes body captures name meta)]))