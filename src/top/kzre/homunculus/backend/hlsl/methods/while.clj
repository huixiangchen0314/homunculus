(ns top.kzre.homunculus.backend.hlsl.methods.while
  "HLSL :while 节点发射。返回 :while 标签，循环体内非块级节点统一包裹 :expr-stmt。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]))

(def ^:private block-level-tags
  #{:if :while :expr-stmt :comment :raw})

(defn- ensure-stmts [body]
  (let [stmts (if (and (vector? body) (not (keyword? (first body))))
                body
                [body])]
    (mapv (fn [s]
            (if (or (not (vector? s)) (contains? block-level-tags (first s)))
              s
              [:expr-stmt s]))
          stmts)))

(defmethod core/emit-node :while [node context]
  (let [test-ast (core/emit-node (n/while-test node) context)
        body-ast (ensure-stmts (core/emit-node (n/while-body node) context))]
    [:while test-ast body-ast]))