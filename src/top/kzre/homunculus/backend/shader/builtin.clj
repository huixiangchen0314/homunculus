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
  '{;; 代数
    +      (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    -      (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    *      (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    /      (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))

    ;; 矩阵/向量乘法 (mul)
    mul    (fn-> (t/->TCon :float4x4) (t/->TCon :float4) (t/->TCon :float4))

    ;; 数学
    abs    (fn-> (t/->TCon :float) (t/->TCon :float))
    max    (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    min    (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    clamp  (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    saturate (fn-> (t/->TCon :float) (t/->TCon :float))

    ;; 三角函数
    sin    (fn-> (t/->TCon :float) (t/->TCon :float))
    cos    (fn-> (t/->TCon :float) (t/->TCon :float))
    tan    (fn-> (t/->TCon :float) (t/->TCon :float))
    asin   (fn-> (t/->TCon :float) (t/->TCon :float))
    acos   (fn-> (t/->TCon :float) (t/->TCon :float))
    atan   (fn-> (t/->TCon :float) (t/->TCon :float))
    atan2  (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))

    ;; 指数/幂
    pow    (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    sqrt   (fn-> (t/->TCon :float) (t/->TCon :float))
    rsqrt  (fn-> (t/->TCon :float) (t/->TCon :float))

    ;; 插值
    lerp   (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    step   (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    smoothstep (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))

    ;; 几何
    dot       (fn-> (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    cross     (fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float3))
    normalize (fn-> (t/->TCon :float3) (t/->TCon :float3))
    length    (fn-> (t/->TCon :float) (t/->TCon :float3))
    distance  (fn-> (t/->TCon :float) (t/->TCon :float3) (t/->TCon :float3))
    reflect   (fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float3))
    refract   (fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float))

    ;; 纹理采样（需要纹理类型作为第一个参数，这里仅给出简化版本）
    sample    (fn-> (t/->TCon :texture2D) (t/->TCon :sampler) (t/->TCon :float2) (t/->TCon :float4))

    ;; 类型转换
    float    (fn-> (t/->TCon :float)  (t/->TCon :int))
    float2   (fn-> (t/->TCon :float2) (t/->TCon :float) (t/->TCon :float))
    float3   (fn-> (t/->TCon :float3) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    float4   (fn-> (t/->TCon :float4) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float) (t/->TCon :float))
    })