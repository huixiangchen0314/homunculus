(ns top.kzre.homunculus.core.ir2.forms.define
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :def [node env]
  (let [meta     (ir2/ir1-meta node)
        kids     (ir1p/children node)   ;; [name-ir, doc-ir?, attr-ir?, val-ir?]
        name-ir  (first kids)
        has-val  (> (count kids) 1)     ;; 除了 name-ir 还有其他子节点，则存在 val
        val-ir   (when has-val (last kids))
        name-node (first (ir2/lower-ast name-ir env))
        val-node  (when val-ir (first (ir2/lower-ast val-ir env)))
        children  (vec (if val-node [name-node val-node] [name-node]))]
    [(m/->DefineNode (:name node) val-node (:doc node) (:attr node) meta children nil)]))