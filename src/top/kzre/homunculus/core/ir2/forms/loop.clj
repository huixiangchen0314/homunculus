(ns top.kzre.homunculus.core.ir2.forms.loop
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :loop [node env]
  (let [bindings   (n1/loop-bindings node)          ;; 扁平绑定列表
        body       (n1/loop-body node)              ;; 单个 IR1 节点（已包装）
        ir-bindings (mapv (fn [{:keys [var val]}]
                            (n2/make-binding (first (ir2/lower-ast var env))
                                             (first (ir2/lower-ast val env))))
                          bindings)
        ir-body     (first (ir2/lower-ast body env))]
    [(n2/make-loop ir-bindings ir-body {} (n1/node-meta node))]))

(defmethod ir2/lower-ast :recur [node env]
  (let [args (mapv #(first (ir2/lower-ast % env)) (n1/recur-exprs node))]
    [(n2/make-recur args {} (n1/node-meta node))]))