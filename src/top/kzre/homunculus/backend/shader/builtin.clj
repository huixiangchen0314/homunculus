(ns top.kzre.homunculus.backend.shader.builtin
  "着色器通用内置函数（HLSL / GLSL / Unity / Unreal 最大公共子集）。
   提供函数名到 IType 的映射，供前端类型推导使用。
   各后端可在此基础上添加专有函数。"
  (:require [top.kzre.homunculus.core.types.type :as ty]))

;; ── 辅助函数：快速构建函数类型 ──────────
(defn- fn-> [& args]
  (reduce (fn [ret arg] (ty/make-tfun arg ret))
          (last args)
          (reverse (butlast args))))

;; ── 内置函数表 ──────────────────────────
;; 键：符号（如 'mul）
;; 值：IType 实例（通常为 TFun）
(def common-builtins
  {;; 代数
   '+ [(fn-> (ty/make-tcon :float)  (ty/make-tcon :float)  (ty/make-tcon :float))
       (fn-> (ty/make-tcon :float4) (ty/make-tcon :float4) (ty/make-tcon :float4))
       (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float3))]
   '-      (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   '* [(fn-> (ty/make-tcon :float)  (ty/make-tcon :float)  (ty/make-tcon :float))
       (fn-> (ty/make-tcon :float4) (ty/make-tcon :float)  (ty/make-tcon :float4))
       ;(fn-> (ty/make-tcon :float4) (ty/make-tcon :float4) (ty/make-tcon :float4))
       (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float3))]
   '/      (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))

    ;; 矩阵/向量乘法 (mul)
   'mul    (fn-> (ty/make-tcon :float4x4) (ty/make-tcon :float4) (ty/make-tcon :float4))

    ;; 数学
   'abs    (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'max    (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'min    (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'clamp  (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'saturate (fn-> (ty/make-tcon :float) (ty/make-tcon :float))

    ;; 三角函数
   'sin    (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'cos    (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'tan    (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'asin   (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'acos   (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'atan   (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'atan2  (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))

    ;; 指数/幂
   'pow    (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'sqrt   (fn-> (ty/make-tcon :float) (ty/make-tcon :float))
   'rsqrt  (fn-> (ty/make-tcon :float) (ty/make-tcon :float))

    ;; 插值
   'lerp   (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'step   (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'smoothstep (fn-> (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))

    ;; 几何
   'dot       (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float))
   'cross     (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float3))
   'normalize (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3))
   'length    (fn-> (ty/make-tcon :float) (ty/make-tcon :float3))
   'distance  (fn-> (ty/make-tcon :float) (ty/make-tcon :float3) (ty/make-tcon :float3))
   'reflect   (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float3))
   'refract   (fn-> (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float3) (ty/make-tcon :float))

    ;; 纹理采样（需要纹理类型作为第一个参数，这里仅给出简化版本）
   'sample    (fn-> (ty/make-tcon :texture2D) (ty/make-tcon :sampler) (ty/make-tcon :float2) (ty/make-tcon :float4))

    ;; 类型转换
   'float    (fn-> (ty/make-tcon :float)  (ty/make-tcon :int))
   'float2   (fn-> (ty/make-tcon :float2) (ty/make-tcon :float) (ty/make-tcon :float))
   'float3   (fn-> (ty/make-tcon :float3) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))
   'float4   (fn-> (ty/make-tcon :float4) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float) (ty/make-tcon :float))


    ;; DSL 资源构造函数（提供类型以通过推导，返回值用于资源分类）
   'texture2D     (fn-> (ty/make-tcon :texture2D))
   'sampler-state (fn-> (ty/make-tcon :sampler))
   'cbuffer       (fn-> (ty/make-tcon :cbuffer))
    })