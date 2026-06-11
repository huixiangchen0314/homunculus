;; ═══════════════════════════════════════════════════════
;; ir2/forms/lambda.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.forms.lambda
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :fn* [node env]
  (let [node-meta (ir2/ir1-meta node)
        name (:name node)
        params (:params node)   ;; [{:sym s :meta m} ...]
        ;; 构造参数 VariableNode
        param-nodes (mapv (fn [p]
                            (m/->VariableNode (name (:sym p)) nil (:meta p) [] nil))
                          params)
        body-start (if name 2 1)
        body-irs (drop body-start (ir1p/children node))
        body (if (= (count body-irs) 1)
               (first (ir2/lower-ast (first body-irs) env))
               (m/->BlockNode (mapv #(first (ir2/lower-ast % env)) body-irs) nil nil
                              (mapv #(first (ir2/lower-ast % env)) body-irs) nil))
        children (vec (concat param-nodes [body]))]
    [(m/->LambdaNode param-nodes body [] name nil node-meta children nil)]))