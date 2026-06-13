(ns top.kzre.homunculus.core.ir2.forms.lambda
  (:require [top.kzre.homunculus.core.ir1.model :as ir1m]
            [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :fn [node env]
  (let [name       (:name node)
        params     (:params node)
        body       (:body node)
        ;; 兼容两种参数格式：{:sym :meta} map 或直接是 IR1 节点 ;; TODO 检查这个合理性，这两种形式是否可能同时存在
        param-nodes (mapv (fn [p]
                            (if (satisfies? ir1p/INode p)
                              (first (ir2/lower-ast p env))
                              (let [sym-node (ir1m/->SymbolNode (:sym p) (:meta p) nil)]
                                (first (ir2/lower-ast sym-node env)))))
                          params)
        body-nodes  (mapv #(first (ir2/lower-ast % env)) body)
        name-node   (when name (first (ir2/lower-ast name env)))
        captures    []
        meta        (ir2/ir1-meta node)
        ir2-body    (if (= 1 (count body-nodes))
                      (first body-nodes)
                      (m/->BlockNode body-nodes nil nil nil))]
    [(m/->LambdaNode param-nodes ir2-body captures name-node nil meta nil)]))