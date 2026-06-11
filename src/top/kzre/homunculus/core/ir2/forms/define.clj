(ns top.kzre.homunculus.core.ir2.forms.define
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :def [ir1-vec env]
  ;; IR1 :def 节点: [node name-ir doc-ir? attr-ir? val-ir?]
  (let [node    (first ir1-vec)
        name    (:name node)                ; 符号
        val-ir  (when (:val node)            ; 值 IR1 向量（可能为 nil）
                  (ir2/lower-ast (:val node) env))
        val     (if val-ir (first val-ir)    ; 提取 lowered 后的第一个节点
                           (ir2/literal nil nil))   ; 无值则生成 nil 字面量
        doc     (:doc node)
        attrs   (:attr node)
        meta    (ir2/ir1-meta ir1-vec)]
    [(ir2/define-expr name val doc attrs meta)]))