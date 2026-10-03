(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端：实现 IFrontendInfo 协议，提供 HLSL 类型、字面量、内置函数。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.symbol :as sym]))


;; ── 用 DSL 构建完整的内置符号表 ──
(defonce ^:private symbol-tables
         (sym/build-symbol-table

           ;;; ========= 类型注册 ===========================================
           ;;; 标量 / 向量 / 矩阵：全小写（与 HLSL 原始拼写一致）
           ;;; 纹理 / 采样器 / 缓冲：PascalCase（与 HLSL 原始拼写一致）

           ;; ═══════════════════════════════════════════
           ;; 原始类型（无字段，不参与 swizzle）
           ;; ═══════════════════════════════════════════
           [:primitive 'bool]
           [:primitive 'int]
           [:primitive 'uint]
           [:primitive 'float]

           ;; 纹理
           [:primitive 'Texture1D]
           [:primitive 'Texture1DArray]
           [:primitive 'Texture2D]
           [:primitive 'Texture2DArray]
           [:primitive 'Texture3D]
           [:primitive 'TextureCube]
           [:primitive 'TextureCubeArray]
           [:primitive 'Texture2DMS]
           [:primitive 'Texture2DMSArray]

           ;; 采样器
           [:primitive 'SamplerState]
           [:primitive 'SamplerComparisonState]

           ;; 缓冲
           [:primitive 'cbuffer]
           [:primitive 'Buffer]
           [:primitive 'StructuredBuffer]
           [:primitive 'ByteAddressBuffer]

           ;; ═══════════════════════════════════════════
           ;; 类型记录（支持 swizzle）
           ;; ═══════════════════════════════════════════

           ;; ── 布尔向量 ────────────────────────────────
           [:record 'bool2 ['x 'bool] ['y 'bool]]
           [:record 'bool3 ['x 'bool] ['y 'bool] ['z 'bool]]
           [:record 'bool4 ['x 'bool] ['y 'bool] ['z 'bool] ['w 'bool]]

           ;; ── 整数向量 ────────────────────────────────
           [:record 'int2 ['x 'int] ['y 'int]]
           [:record 'int3 ['x 'int] ['y 'int] ['z 'int]]
           [:record 'int4 ['x 'int] ['y 'int] ['z 'int] ['w 'int]]

           ;; ── 无符号整数向量 ──────────────────────────
           [:record 'uint2 ['x 'uint] ['y 'uint]]
           [:record 'uint3 ['x 'uint] ['y 'uint] ['z 'uint]]
           [:record 'uint4 ['x 'uint] ['y 'uint] ['z 'uint] ['w 'uint]]

           ;; ── 浮点向量 ────────────────────────────────
           [:record 'float2 ['x 'float] ['y 'float]]
           [:record 'float3 ['x 'float] ['y 'float] ['z 'float]]
           [:record 'float4 ['x 'float] ['y 'float] ['z 'float] ['w 'float]]

           ;; ── 矩阵 ────────────────────────────────────
           [:record 'float2x2
            ['_m00 'float] ['_m01 'float]
            ['_m10 'float] ['_m11 'float]]

           [:record 'float2x3
            ['_m00 'float] ['_m01 'float] ['_m02 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float]]

           [:record 'float2x4
            ['_m00 'float] ['_m01 'float] ['_m02 'float] ['_m03 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float] ['_m13 'float]]

           [:record 'float3x2
            ['_m00 'float] ['_m01 'float]
            ['_m10 'float] ['_m11 'float]
            ['_m20 'float] ['_m21 'float]]

           [:record 'float3x3
            ['_m00 'float] ['_m01 'float] ['_m02 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float]
            ['_m20 'float] ['_m21 'float] ['_m22 'float]]

           [:record 'float3x4
            ['_m00 'float] ['_m01 'float] ['_m02 'float] ['_m03 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float] ['_m13 'float]
            ['_m20 'float] ['_m21 'float] ['_m22 'float] ['_m23 'float]]

           [:record 'float4x2
            ['_m00 'float] ['_m01 'float]
            ['_m10 'float] ['_m11 'float]
            ['_m20 'float] ['_m21 'float]
            ['_m30 'float] ['_m31 'float]]

           [:record 'float4x3
            ['_m00 'float] ['_m01 'float] ['_m02 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float]
            ['_m20 'float] ['_m21 'float] ['_m22 'float]
            ['_m30 'float] ['_m31 'float] ['_m32 'float]]

           [:record 'float4x4
            ['_m00 'float] ['_m01 'float] ['_m02 'float] ['_m03 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float] ['_m13 'float]
            ['_m20 'float] ['_m21 'float] ['_m22 'float] ['_m23 'float]
            ['_m30 'float] ['_m31 'float] ['_m32 'float] ['_m33 'float]]

           [:alias '%%+ '+]
           [:alias '%%- '-]
           [:alias '%%< '<]
           [:alias '%%= '=]
           [:alias '%%not= 'not=]


           ;; 算术四则（float + int 重载）
           [:func '+ {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]]
           [:func '- {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]]
           [:func '* {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'float4 'b 'float] 'float4]
            [['a 'float 'b 'float4] 'float4]
            [['a 'float3 'b 'float] 'float3]
            [['a 'float2 'b 'float] 'float2]
            [['a 'int   'b 'int]   'int]]
           [:func '/ {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]]

           ;; 比较运算（float + int 重载）
           [:func '<  {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]
           [:func '<= {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]
           [:func '> {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]
           [:func '>=  {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]
           [:func '= {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]
           [:func 'not= {:pure? true}
            [['a 'float  'b 'float]  'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int    'b 'int]    'bool]]

           ;; 向量构造函数（多重重载）
           [:func 'float4
            [['a 'float 'b 'float 'c 'float 'd 'float] 'float4]
            [['a 'float2 'b 'float 'c 'float] 'float4]
            [['a 'float3 'b 'float] 'float4]
            [['a 'float 'b 'float3] 'float4]]
           [:func 'float3
            [['a 'float 'b 'float 'c 'float] 'float3]
            [['a 'float2 'b 'float] 'float3]
            [['a 'float 'b 'float2] 'float3]]
           [:func 'float2
            [['a 'float 'b 'float] 'float2]]

           ;; 单重载函数
           [:func 'normalize {:pure? true}
            ['v 'float3] 'float3]
           [:func 'dot {:pure? true}
            ['a 'float3 'b 'float3] 'float]
           [:func 'cross {:pure? true}
            ['a 'float3 'b 'float3] 'float3]
           [:func 'length  {:pure? true}
            ['v 'float3] 'float]
           [:func 'mul    {:pure? true}
            ['a 'float4x4 'b 'float4] 'float4]
           [:func 'max     {:pure? true}
            ['a 'float 'b 'float] 'float]
           [:func 'min      {:pure? true}
            ['a 'float 'b 'float] 'float]
           [:func 'clamp    {:pure? true}
            ['x 'float 'min 'float 'max 'float] 'float]
           [:func 'abs       {:pure? true}
            ['x 'float] 'float]
           [:func 'sin        {:pure? true}
            ['x 'float] 'float]
           [:func 'cos       {:pure? true}
            ['x 'float] 'float]
           [:func 'pow        {:pure? true}
            ['x 'float 'y 'float] 'float]
           [:func 'sqrt      {:pure? true}
            ['x 'float] 'float]
           [:func 'lerp      {:pure? true}
            ['a 'float 'b 'float 't 'float] 'float]
           [:func 'step      {:pure? true}
            ['edge 'float 'x 'float] 'float]
           [:func 'smoothstep  {:pure? true}
            ['min 'float 'max 'float 'x 'float] 'float]
           [:func 'exp    {:pure? true}
            ['x 'float] 'float]
           [:func 'exp2   {:pure? true}
            ['x 'float] 'float]
           [:func 'log   {:pure? true}
            ['x 'float] 'float]
           [:func 'log2   {:pure? true}
            ['x 'float] 'float]
           [:func 'rsqrt {:pure? true}
            ['x 'float] 'float]
           [:func 'frac   {:pure? true}
            ['x 'float] 'float]

           ;; ── 采样函数 ────────────────────────────────
           ;; Texture2D / SamplerState 统一命名（HLSL 原始拼写）
           [:func 'sample {:io? true}
            [['tex 'Texture2D      'samp 'SamplerState 'uv  'float2] 'float4]
            [['tex 'Texture3D      'samp 'SamplerState 'uv  'float3] 'float4]
            [['tex 'TextureCube    'samp 'SamplerState 'dir 'float3] 'float4]
            [['tex 'Texture2DArray 'samp 'SamplerState 'uvw 'float3] 'float4]]
           [:func 'sampleCmp {:io? true}
            [['tex 'Texture2D 'samp 'SamplerComparisonState
              'uv 'float2 'compareValue 'float] 'float]]

           ;; HLSL 特有函数
           [:func 'tex2D {:io? true}
            ['s 'SamplerState 'uv 'float2] 'float4]
           [:func 'tex2Dlod {:io? true}
            ['s 'SamplerState 'uv 'float4] 'float4]
           [:func 'texCUBE {:io? true}
            ['s 'SamplerState 'dir 'float3] 'float4]
           [:func 'clip {:io? true}
            ['x 'float] nil]
           [:func 'discard {:io? true}
            [] nil]
           [:func 'ddx      ['x 'float] 'float]
           [:func 'ddy      ['x 'float] 'float]
           [:func 'fwidth   ['x 'float] 'float]

           ;; ── 类型构造器（零参） ──────────────────────
           ;; 标量
           [:func 'top.kzre.homunculus.backend.shader.dsl/bool  [] 'bool]
           [:func 'top.kzre.homunculus.backend.shader.dsl/int   [] 'int]
           [:func 'top.kzre.homunculus.backend.shader.dsl/uint  [] 'uint]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float [] 'float]

           ;; 向量
           [:func 'top.kzre.homunculus.backend.shader.dsl/bool2  [] 'bool2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/bool3  [] 'bool3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/bool4  [] 'bool4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/int2   [] 'int2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/int3   [] 'int3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/int4   [] 'int4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/uint2  [] 'uint2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/uint3  [] 'uint3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/uint4  [] 'uint4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2 [] 'float2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3 [] 'float3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4 [] 'float4]

           ;; 矩阵
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2x2 [] 'float2x2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2x3 [] 'float2x3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2x4 [] 'float2x4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3x2 [] 'float3x2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3x3 [] 'float3x3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3x4 [] 'float3x4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4x2 [] 'float4x2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4x3 [] 'float4x3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4x4 [] 'float4x4]

           ;; 纹理
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture1D        [] 'Texture1D]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture1DArray   [] 'Texture1DArray]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture2D        [] 'Texture2D]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture2DArray   [] 'Texture2DArray]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture3D        [] 'Texture3D]
           [:func 'top.kzre.homunculus.backend.shader.dsl/TextureCube      [] 'TextureCube]
           [:func 'top.kzre.homunculus.backend.shader.dsl/TextureCubeArray [] 'TextureCubeArray]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture2DMS      [] 'Texture2DMS]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Texture2DMSArray [] 'Texture2DMSArray]

           ;; 采样器
           [:func 'top.kzre.homunculus.backend.shader.dsl/SamplerState           [] 'SamplerState]
           [:func 'top.kzre.homunculus.backend.shader.dsl/SamplerComparisonState [] 'SamplerComparisonState]

           ;; 缓冲
           [:func 'top.kzre.homunculus.backend.shader.dsl/cbuffer           [] 'cbuffer]
           [:func 'top.kzre.homunculus.backend.shader.dsl/Buffer            [] 'Buffer]
           [:func 'top.kzre.homunculus.backend.shader.dsl/StructuredBuffer  [] 'StructuredBuffer]
           [:func 'top.kzre.homunculus.backend.shader.dsl/ByteAddressBuffer [] 'ByteAddressBuffer]
           ))


(defrecord HLSLFrontend []
  tp/IFrontendInfo
  (literal->type [_ val]
    (cond
      (float? val)   (ty/make-tcon 'float)
      (integer? val) (ty/make-tcon 'int)
      (true? val)    (ty/make-tcon 'bool)
      (false? val)   (ty/make-tcon 'bool)
      :else          (ty/make-tvar (gensym "lit"))))

  (builtin-symbols [_] symbol-tables)
  ;; 新增语言约束策略方法
  (truly-type [_] 'bool)    ; HLSL 要求 if 条件为 bool
  (integer-type [_] 'int)
  (macro-namespaces [_] #{'top.kzre.homunculus.backend.shader.dsl
                          'top.kzre.homunculus.core
                          'cljh.core
                          })
  (entry-point? [_ node]
    (boolean (:shader/stage (ir2/node-meta node)))))

(defonce frontend (->HLSLFrontend))