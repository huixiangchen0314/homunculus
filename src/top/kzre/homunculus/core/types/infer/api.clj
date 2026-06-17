(ns top.kzre.homunculus.core.types.infer.api
  "局部类型推断的公共入口。加载所有 defmethod 并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.types.infer.core :as c]
    [top.kzre.homunculus.core.types.infer.methods.assign]
    [top.kzre.homunculus.core.types.infer.methods.block]
    [top.kzre.homunculus.core.types.infer.methods.call]
    [top.kzre.homunculus.core.types.infer.methods.convert]
    [top.kzre.homunculus.core.types.infer.methods.default]
    [top.kzre.homunculus.core.types.infer.methods.define]
    [top.kzre.homunculus.core.types.infer.methods.if]
    [top.kzre.homunculus.core.types.infer.methods.lambda]
    [top.kzre.homunculus.core.types.infer.methods.let]
    [top.kzre.homunculus.core.types.infer.methods.literal]
    [top.kzre.homunculus.core.types.infer.methods.loop]
    [top.kzre.homunculus.core.types.infer.methods.map]
    [top.kzre.homunculus.core.types.infer.methods.member-access]
    [top.kzre.homunculus.core.types.infer.methods.ns]
    [top.kzre.homunculus.core.types.infer.methods.protocol]
    [top.kzre.homunculus.core.types.infer.methods.record]
    [top.kzre.homunculus.core.types.infer.methods.recur]
    [top.kzre.homunculus.core.types.infer.methods.try]      ;; 包含 try / catch / throw
    [top.kzre.homunculus.core.types.infer.methods.variable]
    [top.kzre.homunculus.core.types.infer.methods.vector]
    [top.kzre.homunculus.core.types.infer.methods.while]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defn make-context
  "构建局部推断所需的上下文 map。合并前端内置符号表与编译上下文用户符号表。"
  [compile-ctx frontend ]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)]
    {:frontend frontend
     :ctx compile-ctx
     :symbol-table (merge builtin-table user-table)}))

;; re-export 核心入口
(def infer c/infer)