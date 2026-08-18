(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端：实现 IFrontendInfo 协议，提供 HLSL 类型、字面量、内置函数。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]))


;; ── 用 DSL 构建完整的内置符号表 ──
(defonce ^:private symbol-tables
         (sym/build-symbol-table

           ;; 原始类型
         [:primitive 'float]
         [:primitive 'int]
         [:primitive 'bool]
         [:primitive 'texture2D]
         [:primitive 'sampler]
         [:primitive 'cbuffer]

          [:alias '%%+ '+]
          [:alias '%%< '<]
          [:alias '%%= '=]
          [:alias '%%not= 'not=]

           ;; 类型记录（字段类型用符号）
           [:record 'float4   ['x 'float] ['y 'float] ['z 'float] ['w 'float]]
           [:record 'float3   ['x 'float] ['y 'float] ['z 'float]]
           [:record 'float2   ['x 'float] ['y 'float]]

           ;; 手动枚举 float4x4 的所有 16 个分量
           [:record 'float4x4
            ['_m00 'float] ['_m01 'float] ['_m02 'float] ['_m03 'float]
            ['_m10 'float] ['_m11 'float] ['_m12 'float] ['_m13 'float]
            ['_m20 'float] ['_m21 'float] ['_m22 'float] ['_m23 'float]
            ['_m30 'float] ['_m31 'float] ['_m32 'float] ['_m33 'float]]

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
            [['a 'int    'b 'int]    'bool]
            ]
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
           [:func '>= [['a 'float  'b 'float]  'bool]
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
           [:func 'float3 [['a 'float 'b 'float 'c 'float] 'float3]
            [['a 'float2 'b 'float] 'float3]
            [['a 'float 'b 'float2] 'float3]]
           [:func 'float2 [['a 'float 'b 'float] 'float2]]

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
           [:func 'sample {:io? true}
            ['tex 'texture2D 'samp 'sampler 'uv 'float2] 'float4]
           [:func 'max       ['a 'float 'b 'float] 'float]
           [:func 'min       ['a 'float 'b 'float] 'float]
           [:func 'clamp     ['x 'float 'min 'float 'max 'float] 'float]
           [:func 'abs       ['x 'float] 'float]
           [:func 'sin       ['x 'float] 'float]
           [:func 'cos       ['x 'float] 'float]
           [:func 'pow       ['x 'float 'y 'float] 'float]
           [:func 'sqrt      ['x 'float] 'float]
           [:func 'lerp      ['a 'float 'b 'float 't 'float] 'float]
           [:func 'step      ['edge 'float 'x 'float] 'float]
           [:func 'smoothstep ['min 'float 'max 'float 'x 'float] 'float]
           [:func 'exp   ['x 'float] 'float]
           [:func 'exp2  ['x 'float] 'float]
           [:func 'log   ['x 'float] 'float]
           [:func 'log2  ['x 'float] 'float]
           [:func 'rsqrt ['x 'float] 'float]
           [:func 'frac  ['x 'float] 'float]

           ;; HLSL 特有函数
           [:func 'tex2D  {:io? true}
            ['s 'sampler 'uv 'float2] 'float4]
           [:func 'tex2Dlod {:io? true}
            ['s 'sampler 'uv 'float4] 'float4]
           [:func 'texCUBE  {:io? true}
            ['s 'sampler 'dir 'float3] 'float4]
           [:func 'clip  {:io? true}
            ['x 'float] nil]
           [:func 'discard {:io? true}
            [] nil]
           [:func 'ddx      ['x 'float] 'float]
           [:func 'ddy      ['x 'float] 'float]
           [:func 'fwidth   ['x 'float] 'float]

           ;; 构造函数别名（返回类型也用符号）带命名空间，避免和默认 float 混淆
           [:func 'top.kzre.homunculus.backend.shader.dsl/float    [] 'float]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2   [] 'float2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3   [] 'float3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4   [] 'float4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4x4 [] 'float4x4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/texture2D     [] 'texture2D]
           [:func 'top.kzre.homunculus.backend.shader.dsl/sampler-state [] 'sampler]
           [:func 'top.kzre.homunculus.backend.shader.dsl/cbuffer       [] 'cbuffer]
           [:func `top.kzre.homunculus.backend.shader.dsl/int           [] 'int]
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