(ns top.kzre.homunculus.core.types.lambda-inline.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :let [node config]
  ;; 先递归处理子节点（绑定值和 body），然后尝试内联
  (let [new-bindings (mapv (fn [[v e]]
                             [(inline/eliminate-inline v config)
                              (inline/eliminate-inline e config)])
                           (n/let-bindings node))
        new-body     (inline/eliminate-inline (n/let-body node) config)
        processed    (n/make-let new-bindings new-body
                                 (n/attrs node) (n/node-meta node) (n/parent node))]
    (inline/inline-let processed config)))