(ns top.kzre.homunculus.backend.hlsl.methods.vector
  "HLSL :vector 节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [clojure.string :as str]))

(defmethod core/emit-node :vector [node context]
  (let [items    (n/vector-items node)
        emitted  (mapv #(core/emit-node % context) items)
        vty      (ty/get-type node)
        elem-kw  (when (ty/hetero-vec? vty) (first (ty/hetero-vec-types vty)))
        type-str (if elem-kw (core/hlsl-type-str elem-kw)
                             (throw (ex-info "Vector element type unknown" {:node node})))]
    ;; 利用 :constructor 标签生成 type(args...)
    (into [:constructor type-str] emitted)))