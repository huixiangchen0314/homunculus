(ns top.kzre.homunculus.core.ir2.forms.lambda
  "lambda / fn 的 IR2 lowering。"
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :fn [node env]
  (let [name        (n1/fn-name node)            ;; SymbolNode 或 nil
        params      (n1/fn-params node)          ;; IR1 Param 向量
        body        (n1/fn-body node)            ;; 单个 IR1 节点
        name-node   (when name (first (ir2/lower-ast name env)))
        ;; 对每个参数执行 lowering，将调用 :param 分支
        param-nodes (mapv #(first (ir2/lower-ast % env)) params)
        body-node   (first (ir2/lower-ast body env))
        captures    []]
    [(n2/make-lambda param-nodes body-node captures name-node
                     {}                         ;; attrs 暂空
                     (n1/node-meta node))]))