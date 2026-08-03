(ns top.kzre.homunculus.core.ir2.forms.let
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :let [node env]
  (let [bindings   (n1/let-bindings node)          ;; 扁平列表 [sym val sym val ...]
        body       (n1/let-body node)              ;; 单个 IR1 节点
        ir-bindings (mapv (fn [{:keys [var val meta]}]
                            (m/->Binding (first (ir2/lower-ast var env))
                                         (first (ir2/lower-ast val env))
                                         {}
                                         meta))
                          bindings)
        ir-body    (first (ir2/lower-ast body env))]
    [(n2/make-let ir-bindings ir-body {} (n1/node-meta node) )]))