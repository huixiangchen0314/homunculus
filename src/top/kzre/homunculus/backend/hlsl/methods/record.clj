(ns top.kzre.homunculus.backend.hlsl.methods.record
  "HLSL :record 节点发射 —— 将 defrecord 编译为 HLSL struct。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.shader.core :as sc]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod core/emit-node :record [node context]
  (let [struct-name (name (n/record-name node))
        fields      (n/record-fields node)
        members     (mapv (fn [field]
                            (let [fname    (name (n/field-name field))
                                  init-expr (n/field-init field)
                                  ;; 获取字段类型字符串
                                  ir-type   (if init-expr
                                              (ty/get-type init-expr)
                                              ;; 从字段元数据推断
                                              (ty/meta->type (:meta field)
                                                             (:known-types context)))
                                  _         (when-not ir-type
                                              (throw (ex-info (str "Record field missing type: " fname)
                                                              {:field field})))
                                  type-str  (core/hlsl-type-str ir-type)
                                  semantic  (sc/semantic-from-meta (n/field-meta field))]
                              [:struct-member type-str fname semantic]))
                          fields)]
    [:struct struct-name members]))