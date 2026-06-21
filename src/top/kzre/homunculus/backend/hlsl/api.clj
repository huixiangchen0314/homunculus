(ns top.kzre.homunculus.backend.hlsl.api
  "HLSL 代码生成公共入口。加载所有发射方法，提供上下文构造与发射函数。"
  (:require
    [top.kzre.homunculus.backend.hlsl.core :as core]
    ;; 加载各方法文件以注册 defmethod
    [top.kzre.homunculus.backend.hlsl.methods.literal]
    [top.kzre.homunculus.backend.hlsl.methods.variable]
    [top.kzre.homunculus.backend.hlsl.methods.call]
    [top.kzre.homunculus.backend.hlsl.methods.if]
    [top.kzre.homunculus.backend.hlsl.methods.block]
    [top.kzre.homunculus.backend.hlsl.methods.while]
    [top.kzre.homunculus.backend.hlsl.methods.assign]
    [top.kzre.homunculus.backend.hlsl.methods.let]
    [top.kzre.homunculus.backend.hlsl.methods.convert]
    [top.kzre.homunculus.backend.hlsl.methods.member-access]
    [top.kzre.homunculus.backend.hlsl.methods.record]
    [top.kzre.homunculus.backend.hlsl.methods.vector]
    [top.kzre.homunculus.backend.hlsl.methods.lambda]
    [top.kzre.homunculus.backend.hlsl.methods.ns]
    [top.kzre.homunculus.backend.hlsl.methods.define]
    [top.kzre.homunculus.backend.hlsl.methods.array]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.internal.utils :as iu]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defn make-context
  "构造 HLSL 发射上下文。
   compile-ctx : 编译上下文（实现 ICompileContext）
   frontend    : 前端实例（实现 IFrontendInfo）"
  [compile-ctx frontend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)
        ;; 从编译上下文中获取编译配置
        exclude-ns (tp/macro-namespaces frontend)
        config        (ip/config compile-ctx)
        style         (when config (ip/module-naming-style config))
        style         (or style :default)
        module-naming-fn (fn [ns-sym] (iu/ns->module-path ns-sym style ".hlsl"))]
    {:frontend         frontend
     :ctx              compile-ctx
     :symbol-table     symbols
     :known-types      (sym/types-symbols symbols)
     :module-naming-fn module-naming-fn
     :exclude-ns       (set exclude-ns)
     }))

(def emit core/emit)