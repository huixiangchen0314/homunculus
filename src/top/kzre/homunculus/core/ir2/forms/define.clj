(ns top.kzre.homunculus.core.ir2.forms.define
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :def [node env]
  (let [node-meta (ir2/ir1-meta node)
        name-sym  (:name node)   ;; 保持为符号
        val-node  (when (:val node)
                    (first (ir2/lower-ast (:val node) env)))
        children  (if val-node [val-node] [])]
    [(m/->DefineNode name-sym val-node (:doc node) (:attr node) node-meta children nil)]))