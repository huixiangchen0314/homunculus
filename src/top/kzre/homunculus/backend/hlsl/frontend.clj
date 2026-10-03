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


           ;; ═══════════════════════════════════════════
           ;; 运算符
           ;; ═══════════════════════════════════════════

           ;; ── 算术四则 ────────────────────────────────
           [:func '+ {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]
            [['a 'uint  'b 'uint]  'uint]]
           [:func '- {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]
            [['a 'uint  'b 'uint]  'uint]]
           [:func '* {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'float4 'b 'float] 'float4]
            [['a 'float 'b 'float4] 'float4]
            [['a 'float3 'b 'float] 'float3]
            [['a 'float2 'b 'float] 'float2]
            [['a 'int   'b 'int]   'int]
            [['a 'uint  'b 'uint]  'uint]]
           [:func '/ {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int   'b 'int]   'int]
            [['a 'uint  'b 'uint]  'uint]]

           ;; ── 位运算 ──────────────────────────────────
           [:func '& {:pure? true}
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func '| {:pure? true}
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func '^ {:pure? true}
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func '~ {:pure? true}
            [['a 'int] 'int]
            [['a 'uint] 'uint]]
           [:func '<< {:pure? true}
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func '>> {:pure? true}
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]

           ;; ── 逻辑运算 ────────────────────────────────
           [:func '&& {:pure? true}
            [['a 'bool 'b 'bool] 'bool]]
           [:func '|| {:pure? true}
            [['a 'bool 'b 'bool] 'bool]]
           [:func '! {:pure? true}
            [['a 'bool] 'bool]]

           ;; ── 比较运算 ────────────────────────────────
           [:func '<  {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]
           [:func '<= {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]
           [:func '> {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]
           [:func '>= {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]
           [:func '= {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]
           [:func 'not= {:pure? true}
            [['a 'float 'b 'float] 'bool]
            [['a 'float2 'b 'float2] 'bool]
            [['a 'float3 'b 'float3] 'bool]
            [['a 'float4 'b 'float4] 'bool]
            [['a 'int 'b 'int] 'bool]
            [['a 'uint 'b 'uint] 'bool]]

           ;; ═══════════════════════════════════════════
           ;; 向量构造
           ;; ═══════════════════════════════════════════
           [:func 'float2
            [['a 'float 'b 'float] 'float2]]
           [:func 'float3
            [['a 'float 'b 'float 'c 'float] 'float3]
            [['a 'float2 'b 'float] 'float3]
            [['a 'float 'b 'float2] 'float3]]
           [:func 'float4
            [['a 'float 'b 'float 'c 'float 'd 'float] 'float4]
            [['a 'float2 'b 'float 'c 'float] 'float4]
            [['a 'float3 'b 'float] 'float4]
            [['a 'float 'b 'float3] 'float4]]

           [:func 'int2
            [['a 'int 'b 'int] 'int2]]
           [:func 'int3
            [['a 'int 'b 'int 'c 'int] 'int3]
            [['a 'int2 'b 'int] 'int3]]
           [:func 'int4
            [['a 'int 'b 'int 'c 'int 'd 'int] 'int4]
            [['a 'int2 'b 'int 'c 'int] 'int4]
            [['a 'int3 'b 'int] 'int4]]

           [:func 'uint2
            [['a 'uint 'b 'uint] 'uint2]]
           [:func 'uint3
            [['a 'uint 'b 'uint 'c 'uint] 'uint3]
            [['a 'uint2 'b 'uint] 'uint3]]
           [:func 'uint4
            [['a 'uint 'b 'uint 'c 'uint 'd 'uint] 'uint4]
            [['a 'uint2 'b 'uint 'c 'uint] 'uint4]
            [['a 'uint3 'b 'uint] 'uint4]]

           [:func 'bool2
            [['a 'bool 'b 'bool] 'bool2]]
           [:func 'bool3
            [['a 'bool 'b 'bool 'c 'bool] 'bool3]
            [['a 'bool2 'b 'bool] 'bool3]]
           [:func 'bool4
            [['a 'bool 'b 'bool 'c 'bool 'd 'bool] 'bool4]
            [['a 'bool2 'b 'bool 'c 'bool] 'bool4]
            [['a 'bool3 'b 'bool] 'bool4]]

           ;; ═══════════════════════════════════════════
           ;; 数学函数
           ;; ═══════════════════════════════════════════

           ;; ── 一元函数 ────────────────────────────────
           [:func 'saturate {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'sign {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]
            [['x 'int]    'int]
            [['x 'uint]   'uint]]
           [:func 'floor {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'ceil {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'round {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'trunc {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'frac {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'abs {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]
            [['x 'int]    'int]
            [['x 'uint]   'uint]]
           [:func 'sqrt {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'rsqrt {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'exp {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'exp2 {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'log {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'log2 {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'sin {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'cos {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'tan {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'asin {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'acos {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'sinh {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'cosh {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'tanh {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'degrees {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'radians {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]

           ;; ── 二元函数 ────────────────────────────────
           [:func 'atan {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]
            [['y 'float 'x 'float] 'float]
            [['y 'float2 'x 'float2] 'float2]
            [['y 'float3 'x 'float3] 'float3]
            [['y 'float4 'x 'float4] 'float4]]
           [:func 'pow {:pure? true}
            [['x 'float 'y 'float] 'float]
            [['x 'float2 'y 'float2] 'float2]
            [['x 'float3 'y 'float3] 'float3]
            [['x 'float4 'y 'float4] 'float4]]
           [:func 'fmod {:pure? true}
            [['x 'float 'y 'float] 'float]
            [['x 'float2 'y 'float2] 'float2]
            [['x 'float3 'y 'float3] 'float3]
            [['x 'float4 'y 'float4] 'float4]]
           [:func 'max {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func 'min {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'int 'b 'int] 'int]
            [['a 'uint 'b 'uint] 'uint]]
           [:func 'step {:pure? true}
            [['edge 'float 'x 'float] 'float]
            [['edge 'float2 'x 'float2] 'float2]
            [['edge 'float3 'x 'float3] 'float3]
            [['edge 'float4 'x 'float4] 'float4]]

           ;; ── 三元函数 ────────────────────────────────
           [:func 'clamp {:pure? true}
            [['x 'float 'min 'float 'max 'float] 'float]
            [['x 'float2 'min 'float2 'max 'float2] 'float2]
            [['x 'float3 'min 'float3 'max 'float3] 'float3]
            [['x 'float4 'min 'float4 'max 'float4] 'float4]
            [['x 'int 'min 'int 'max 'int] 'int]
            [['x 'uint 'min 'uint 'max 'uint] 'uint]]
           [:func 'lerp {:pure? true}
            [['a 'float 'b 'float 't 'float] 'float]
            [['a 'float2 'b 'float2 't 'float2] 'float2]
            [['a 'float3 'b 'float3 't 'float3] 'float3]
            [['a 'float4 'b 'float4 't 'float4] 'float4]]
           [:func 'smoothstep {:pure? true}
            [['min 'float 'max 'float 'x 'float] 'float]
            [['min 'float2 'max 'float2 'x 'float2] 'float2]
            [['min 'float3 'max 'float3 'x 'float3] 'float3]
            [['min 'float4 'max 'float4 'x 'float4] 'float4]]
           [:func 'mad {:pure? true}
            [['a 'float 'b 'float 'c 'float] 'float]
            [['a 'float2 'b 'float2 'c 'float2] 'float2]
            [['a 'float3 'b 'float3 'c 'float3] 'float3]
            [['a 'float4 'b 'float4 'c 'float4] 'float4]]

           ;; ═══════════════════════════════════════════
           ;; 几何函数
           ;; ═══════════════════════════════════════════
           [:func 'normalize {:pure? true}
            [['v 'float]  'float]
            [['v 'float2] 'float2]
            [['v 'float3] 'float3]
            [['v 'float4] 'float4]]
           [:func 'length {:pure? true}
            [['v 'float]  'float]
            [['v 'float2] 'float]
            [['v 'float3] 'float]
            [['v 'float4] 'float]]
           [:func 'distance {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float]
            [['a 'float3 'b 'float3] 'float]
            [['a 'float4 'b 'float4] 'float]]
           [:func 'dot {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float]
            [['a 'float3 'b 'float3] 'float]
            [['a 'float4 'b 'float4] 'float]]
           [:func 'cross {:pure? true}
            [['a 'float3 'b 'float3] 'float3]]
           [:func 'reflect {:pure? true}
            [['i 'float2 'n 'float2] 'float2]
            [['i 'float3 'n 'float3] 'float3]
            [['i 'float4 'n 'float4] 'float4]]
           [:func 'refract {:pure? true}
            [['i 'float2 'n 'float2 'eta 'float] 'float2]
            [['i 'float3 'n 'float3 'eta 'float] 'float3]
            [['i 'float4 'n 'float4 'eta 'float] 'float4]]
           [:func 'faceforward {:pure? true}
            [['n 'float2 'i 'float2 'ng 'float2] 'float2]
            [['n 'float3 'i 'float3 'ng 'float3] 'float3]
            [['n 'float4 'i 'float4 'ng 'float4] 'float4]]

           ;; ═══════════════════════════════════════════
           ;; 矩阵乘法
           ;; ═══════════════════════════════════════════
           [:func 'mul {:pure? true}
            [['a 'float 'b 'float] 'float]
            [['a 'float2 'b 'float2] 'float2]
            [['a 'float3 'b 'float3] 'float3]
            [['a 'float4 'b 'float4] 'float4]
            [['a 'float4x4 'b 'float4] 'float4]
            [['a 'float4 'b 'float4x4] 'float4]
            [['a 'float4x4 'b 'float4x4] 'float4x4]
            [['a 'float3x3 'b 'float3] 'float3]
            [['a 'float3 'b 'float3x3] 'float3]
            [['a 'float3x3 'b 'float3x3] 'float3x3]]

           ;; ═══════════════════════════════════════════
           ;; 纹理采样
           ;; ═══════════════════════════════════════════
           [:func 'sample {:io? true}
            [['tex 'Texture1D        'samp 'SamplerState 'uv  'float]  'float4]
            [['tex 'Texture2D        'samp 'SamplerState 'uv  'float2] 'float4]
            [['tex 'Texture3D        'samp 'SamplerState 'uv  'float3] 'float4]
            [['tex 'TextureCube      'samp 'SamplerState 'dir 'float3] 'float4]
            [['tex 'Texture1DArray   'samp 'SamplerState 'uv  'float2] 'float4]
            [['tex 'Texture2DArray   'samp 'SamplerState 'uvw 'float3] 'float4]
            [['tex 'TextureCubeArray 'samp 'SamplerState 'uvw 'float4] 'float4]]
           [:func 'sampleLod {:io? true}
            [['tex 'Texture1D      'samp 'SamplerState 'uv  'float  'lod 'float] 'float4]
            [['tex 'Texture2D      'samp 'SamplerState 'uv  'float2 'lod 'float] 'float4]
            [['tex 'Texture3D      'samp 'SamplerState 'uv  'float3 'lod 'float] 'float4]
            [['tex 'TextureCube    'samp 'SamplerState 'dir 'float3 'lod 'float] 'float4]
            [['tex 'Texture2DArray 'samp 'SamplerState 'uvw 'float3 'lod 'float] 'float4]]
           [:func 'sampleCmp {:io? true}
            [['tex 'Texture2D 'samp 'SamplerComparisonState
              'uv 'float2 'compareValue 'float] 'float]]
           [:func 'sampleCmpLod {:io? true}
            [['tex 'Texture2D 'samp 'SamplerComparisonState
              'uv 'float2 'compareValue 'float 'lod 'float] 'float]]

           ;; ═══════════════════════════════════════════
           ;; HLSL 特有函数
           ;; ═══════════════════════════════════════════
           [:func 'tex1D {:io? true}
            [['s 'SamplerState 'uv 'float] 'float4]]
           [:func 'tex2D {:io? true}
            [['s 'SamplerState 'uv 'float2] 'float4]]
           [:func 'tex3D {:io? true}
            [['s 'SamplerState 'uvw 'float3] 'float4]]
           [:func 'texCUBE {:io? true}
            [['s 'SamplerState 'dir 'float3] 'float4]]
           [:func 'tex1Dlod {:io? true}
            [['s 'SamplerState 'uv 'float2] 'float4]]
           [:func 'tex2Dlod {:io? true}
            [['s 'SamplerState 'uv 'float4] 'float4]]
           [:func 'tex3Dlod {:io? true}
            [['s 'SamplerState 'uvw 'float4] 'float4]]
           [:func 'texCUBElod {:io? true}
            [['s 'SamplerState 'dir 'float4] 'float4]]
           [:func 'tex2Dproj {:io? true}
            [['s 'SamplerState 'uv 'float3] 'float4]]
           [:func 'tex2Dbias {:io? true}
            [['s 'SamplerState 'uv 'float4] 'float4]]

           [:func 'clip {:io? true}
            [['x 'float] nil]]
           [:func 'discard {:io? true}
            [] nil]

           [:func 'ddx {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'ddy {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]
           [:func 'fwidth {:pure? true}
            [['x 'float]  'float]
            [['x 'float2] 'float2]
            [['x 'float3] 'float3]
            [['x 'float4] 'float4]]

           ;; ═══════════════════════════════════════════
           ;; 类型转换
           ;; ═══════════════════════════════════════════
           [:func 'asfloat {:pure? true}
            [['x 'int]   'float]
            [['x 'uint]  'float]
            [['x 'int2]  'float2]
            [['x 'uint2] 'float2]
            [['x 'int3]  'float3]
            [['x 'uint3] 'float3]
            [['x 'int4]  'float4]
            [['x 'uint4] 'float4]]
           [:func 'asint {:pure? true}
            [['x 'float]  'int]
            [['x 'uint]   'int]
            [['x 'float2] 'int2]
            [['x 'uint2]  'int2]
            [['x 'float3] 'int3]
            [['x 'uint3]  'int3]
            [['x 'float4] 'int4]
            [['x 'uint4]  'int4]]
           [:func 'asuint {:pure? true}
            [['x 'float]  'uint]
            [['x 'int]    'uint]
            [['x 'float2] 'uint2]
            [['x 'int2]   'uint2]
            [['x 'float3] 'uint3]
            [['x 'int3]   'uint3]
            [['x 'float4] 'uint4]
            [['x 'int4]   'uint4]]

           ;; ═══════════════════════════════════════════
           ;; 类型构造器（零参）
           ;; ═══════════════════════════════════════════
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