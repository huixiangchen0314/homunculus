(ns top.kzre.homunculus.core.ir2.forms.define
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :def [node env]
  (let [meta     (ir2/ir1-meta node)
        ;; 注意：build-tree 已把 :name 转成了 IR1 SymbolNode
        sym-node (:name node)            ; ← SymbolNode
        name-sym (:name sym-node)        ; ← 真正的符号，如 'x
        val      (:val node)             ; IR1 节点或 nil
        val-node (when val (first (ir2/lower-ast val env)))
        ;; 文档字符串和属性映射也已转为 IR1 节点（可忽略或保留原值）
        ;; 我们只需要将名称符号转为 IR2 VariableNode（但不用放入 children）
        ]
    [(m/->DefineNode name-sym val-node (:doc node) (:attr node) meta nil)]))