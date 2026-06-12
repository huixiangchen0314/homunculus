(ns top.kzre.homunculus.core.ir2.forms.lambda
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :fn [node env]
  (let [name       (:name node)           ;; 已是 IR1 节点或 nil
        params     (:params node)         ;; 已是 IR1 节点向量
        body       (:body node)           ;; 已是 IR1 节点向量
        param-nodes (mapv #(first (ir2/lower-ast % env)) params)
        body-nodes  (mapv #(first (ir2/lower-ast % env)) body)
        name-node   (when name (first (ir2/lower-ast name env)))
        captures    []
        meta        (ir2/ir1-meta node)
        ;; 单个 body 表达式直接使用，多个则包装成 block
        ir2-body    (if (= 1 (count body-nodes))
                      (first body-nodes)
                      (m/->BlockNode body-nodes nil nil nil))]
    [(m/->LambdaNode param-nodes ir2-body captures name-node nil meta nil)]))