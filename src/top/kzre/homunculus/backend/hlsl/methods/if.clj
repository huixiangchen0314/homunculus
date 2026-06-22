(ns top.kzre.homunculus.backend.hlsl.methods.if
  "HLSL :if 节点发射。返回 :if 标签，分支内非块级节点统一包裹 :expr-stmt。"
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

(defmethod core/emit-node :if [node context]
  (let [test-ast (core/emit-node (n/if-test node) context)
        then-ast (ensure-stmts (core/emit-node (n/if-then node) context))
        else-ast (when-let [e (n/if-else node)]
                   (ensure-stmts (core/emit-node e context)))]
    [:if test-ast then-ast else-ast]))