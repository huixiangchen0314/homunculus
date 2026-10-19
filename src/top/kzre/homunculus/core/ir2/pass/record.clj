(ns top.kzre.homunculus.core.ir2.pass.record
  "record / deftype 自动构造器的名称和类型生成。

   本命名空间只做纯计算——给定 record 名和字段类型，返回构造器的
   符号名和函数类型。注册、绑定、emit 由调用方处理。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn make-ctor
  "生成 record 位置参数构造器的名称和类型。

   record-name —— record 的符号名（可能带命名空间前缀）
   arg-types   —— 字段类型向量，按字段声明顺序

   返回 {:name 构造器符号 :type 函数类型}。

   构造器名 ->RecordName 继承 record-name 的命名空间：
     VertOutput     → ->VertOutput
     ns/VertOutput  → ns/->VertOutput

   函数类型——从 arg-types 到 (TCon record-name) 的柯里化函数：
     [float4 float float] → (Tfun float4 (Tfun float (Tfun float (TCon record-name))))"
  [record-name arg-types]
  {:name (symbol (namespace record-name)
                 (str "->" (name record-name)))
   :type
   (ty/make-fun-type arg-types (ty/make-tcon record-name))})