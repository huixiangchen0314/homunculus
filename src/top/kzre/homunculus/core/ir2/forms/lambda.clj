(ns top.kzre.homunculus.core.ir2.forms.lambda
  "lambda / fn 的 IR2 lowering。"
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :fn [node env]
  ;; 参数在 build-tree 后已全部转为 SymbolNode，无需兼容旧格式
  (let [name        (n1/fn-name node)            ;; SymbolNode 或 nil
        params      (n1/fn-params node)          ;; SymbolNode 向量
        body        (n1/fn-body node)            ;; 单个 IR1 节点（可能为 DoNode）
        name-node   (when name (first (ir2/lower-ast name env)))
        param-nodes (mapv #(first (ir2/lower-ast % env)) params)
        body-node   (first (ir2/lower-ast body env))
        captures    []]
    [(n2/make-lambda param-nodes body-node captures name-node
                     {}                         ;; attrs 暂空
                     (n1/node-meta node)
                     nil)]))