(ns top.kzre.homunculus.backend.hlsl.methods.record
  "HLSL :record 节点发射 —— 将 defrecord 编译为 HLSL struct。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [clojure.string :as str]))

(defmethod core/emit-node :record [node]
  (let [struct-name (name (n/record-name node))
        fields      (n/record-fields node)
        members     (mapv (fn [field]
                            (let [fname (name (n/field-name field))
                                  ftype (n/field-init field)  ;; 字段的初始值表达式，其类型即字段类型
                                  ir-type (when ftype (ty/get-type ftype))]
                              (when-not ir-type
                                (throw (ex-info (str "Record field missing type: " fname)
                                                {:field field})))
                              (tmpl/struct-member (core/hlsl-type-str ir-type) fname nil)))
                          fields)
        members-str (str/join "\n" members)]
    (tmpl/struct-decl struct-name members-str)))