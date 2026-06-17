(ns top.kzre.homunculus.backend.hlsl.methods.let
  "HLSL :let 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [clojure.string :as str]))

(defmethod core/emit-node :let [node]
  (let [bindings (n/let-bindings node)
        decls    (mapv (fn [[v e]]
                         (let [v-type (core/hlsl-type-str (ty/get-type v))]
                           (str v-type " " (name (n/var-name v)) " = " (core/emit-node e) ";")))
                       bindings)
        body     (core/emit-node (n/let-body node))]
    (str (str/join "\n" decls) "\n" body)))