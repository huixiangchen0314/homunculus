(ns top.kzre.homunculus.backend.hlsl.methods.let
  "HLSL :let 节点发射。返回无标签向量，所有绑定声明作为语句（:expr-stmt 包裹）。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.backend.util.naming :refer [cname]]))

(defmethod core/emit-node :let [node context]
  (let [bindings (n/let-bindings node)
        body     (n/let-body node)
        decls (mapv (fn [[v e]]
                      (let [ir-type (ty/get-type v)]
                        (if (ty/vec-type? ir-type)
                          [:expr-stmt
                           [:array-decl
                            (core/hlsl-type-str (ty/vec-element-type ir-type))
                            (cname (n/var-name v))
                            [:literal (ty/vec-size ir-type)]]]
                          [:expr-stmt
                           [:var-decl
                            (core/hlsl-type-str ir-type)
                            (cname (n/var-name v))
                            (core/emit-node e context)]])))
                    bindings)
        body-ast (core/emit-node body context)]
    (into decls (if (and (vector? body-ast) (not (keyword? (first body-ast))))
                  body-ast
                  [body-ast]))))