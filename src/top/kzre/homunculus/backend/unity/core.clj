(ns top.kzre.homunculus.backend.unity.core
  "Unity ShaderLab 后端。复用 HLSL 编译器，将生成的 HLSL 代码包装为 ShaderLab 格式。"
  (:require
    [top.kzre.homunculus.backend.hlsl.config :as hlsl]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [clojure.string :as str]))

(defn- wrap-shaderlab
  "将 HLSL 代码字符串包装为 Unity ShaderLab 文件内容。
   hlsl-code  : 完整的 HLSL 代码（包含入口包装后的函数）
   shader-name: Shader 资源路径，例如 \"MyShader\""
  [hlsl-code shader-name]
  (str "Shader \"" shader-name "\" {\n"
       "    SubShader {\n"
       "        Pass {\n"
       "            CGPROGRAM\n"
       "            #pragma vertex vert\n"
       "            #pragma fragment frag\n"
       "            #include \"UnityCG.cginc\"\n"
       "            " hlsl-code "\n"
       "            ENDCG\n"
       "        }\n"
       "    }\n"
       "    FallBack \"Diffuse\"\n"
       "}"))

(defrecord UnityShaderLabCompiler []
  p/ICompiler
  (compile-module [this context forms]
    ;; 直接委托给 HLSL 编译器
    (p/compile-module (hlsl/->HLSLCompiler) context forms))

  (link [this context]
    ;; 生成 HLSL 代码
    (let [hlsl-compiler (hlsl/->HLSLCompiler)
          hlsl-code (p/link hlsl-compiler context)
          ;; 获取第一个入口函数名称，用于 ShaderLab 命名
          ;; 这里简单使用 "DefaultShader"
          shader-name "DefaultShader"]
      (wrap-shaderlab hlsl-code shader-name)))

  (emit [this unit context]
    ;; 单个模块发射也可复用
    (let [hlsl-compiler (hlsl/->HLSLCompiler)
          hlsl-code (p/emit hlsl-compiler unit context)
          shader-name (str "Module/" (name (:ns-sym unit)))]
      (wrap-shaderlab hlsl-code shader-name))))