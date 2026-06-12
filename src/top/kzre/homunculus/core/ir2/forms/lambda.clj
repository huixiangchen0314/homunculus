(ns top.kzre.homunculus.core.ir2.forms.lambda
  (:require [top.kzre.homunculus.core.ir1.model :as ir1m]
            [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :fn [node env]
  (let [name       (:name node)
        params     (:params node)
        param-irs  (mapv (fn [p] (ir1m/->SymbolNode (:sym p) (:meta p) [] nil)) params)
        param-nodes (mapv #(first (ir2/lower-ast % env)) param-irs)
        num-params (count params)
        has-name   (some? name)
        body-start (+ num-params (if has-name 1 0))
        body-irs   (drop body-start (ir1p/children node))
        body       (if (= (count body-irs) 1)
                     (first (ir2/lower-ast (first body-irs) env))
                     (m/->BlockNode (mapv #(first (ir2/lower-ast % env)) body-irs)
                                    nil nil (mapv #(first (ir2/lower-ast % env)) body-irs) nil))
        captures   []
        meta       (ir2/ir1-meta node)]
    [(m/->LambdaNode param-nodes body captures name nil meta
                     (vec (concat param-nodes [body])) nil)]))