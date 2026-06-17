(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端：实现 IFrontendInfo 协议，提供 HLSL 类型、字面量、内置函数。"
  (:require
    [top.kzre.homunculus.backend.shader.builtin :as builtin]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.internal.symbol :as sym]
    [top.kzre.homunculus.core.types.type :as ty]))

;; HLSL 前端基本类型
(defonce ^:private tfloat   (ty/make-tcon :float))
(defonce ^:private tfloat2  (ty/make-tcon :float2))
(defonce ^:private tfloat3  (ty/make-tcon :float3))
(defonce ^:private tfloat4  (ty/make-tcon :float4))
(defonce ^:private tfloat4x4 (ty/make-tcon :float4x4))
(defonce ^:private tbool    (ty/make-tcon :bool))
(defonce ^:private tint     (ty/make-tcon :int))
(defonce ^:private ttex     (ty/make-tcon :texture2D))
(defonce ^:private tsampler (ty/make-tcon :sampler))
(defonce ^:private tcbuffer (ty/make-tcon :cbuffer))

;; ── 用 DSL 构建完整的内置符号表 ──
(defonce ^:private symbol-tables
         (sym/build-symbol-table
           ;; 类型记录
           [:record 'float4   ['x :float] ['y :float] ['z :float] ['w :float]]
           [:record 'float3   ['x :float] ['y :float] ['z :float]]
           [:record 'float2   ['x :float] ['y :float]]

           ;; 手动枚举 float4x4 的所有 16 个分量
           [:record 'float4x4
            ['_m00 :float] ['_m01 :float] ['_m02 :float] ['_m03 :float]
            ['_m10 :float] ['_m11 :float] ['_m12 :float] ['_m13 :float]
            ['_m20 :float] ['_m21 :float] ['_m22 :float] ['_m23 :float]
            ['_m30 :float] ['_m31 :float] ['_m32 :float] ['_m33 :float]]

           ;; 多重重载函数（每个 arity 用向量包裹）
           [:func '+ [['a :float 'b :float] :float]
            [['a :float2 'b :float2] :float2]
            [['a :float3 'b :float3] :float3]
            [['a :float4 'b :float4] :float4]]
           [:func '- [['a :float 'b :float] :float]
            [['a :float2 'b :float2] :float2]
            [['a :float3 'b :float3] :float3]
            [['a :float4 'b :float4] :float4]]
           [:func '* [['a :float 'b :float] :float]
            [['a :float2 'b :float2] :float2]
            [['a :float3 'b :float3] :float3]
            [['a :float4 'b :float4] :float4]
            [['a :float4 'b :float] :float4]
            [['a :float3 'b :float] :float3]
            [['a :float2 'b :float] :float2]]
           [:func '/ [['a :float 'b :float] :float]
            [['a :float2 'b :float2] :float2]
            [['a :float3 'b :float3] :float3]
            [['a :float4 'b :float4] :float4]]

           ;; 单重载函数
           [:func 'normalize ['v :float3] :float3]
           [:func 'dot       ['a :float3 'b :float3] :float]
           [:func 'cross     ['a :float3 'b :float3] :float3]
           [:func 'length    ['v :float3] :float]
           [:func 'mul       ['a :float4x4 'b :float4] :float4]
           [:func 'sample    ['tex :texture2D 'samp :sampler 'uv :float2] :float4]
           [:func 'max       ['a :float 'b :float] :float]
           [:func 'min       ['a :float 'b :float] :float]
           [:func 'clamp     ['x :float 'min :float 'max :float] :float]
           [:func 'abs       ['x :float] :float]
           [:func 'sin       ['x :float] :float]
           [:func 'cos       ['x :float] :float]
           [:func 'pow       ['x :float 'y :float] :float]
           [:func 'sqrt      ['x :float] :float]
           [:func 'lerp      ['a :float 'b :float 't :float] :float]
           [:func 'step      ['edge :float 'x :float] :float]
           [:func 'smoothstep ['min :float 'max :float 'x :float] :float]
           ;; HLSL 特有函数
           [:func 'tex2D    ['s :sampler 'uv :float2] :float4]
           [:func 'tex2Dlod ['s :sampler 'uv :float4] :float4]
           [:func 'texCUBE  ['s :sampler 'dir :float3] :float4]
           [:func 'clip     ['x :float] nil]
           [:func 'discard  [] nil]
           [:func 'ddx      ['x :float] :float]
           [:func 'ddy      ['x :float] :float]
           [:func 'fwidth   ['x :float] :float]

           [:func 'top.kzre.homunculus.backend.shader.dsl/float    [] :float]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float2   [] :float2]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float3   [] :float3]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4   [] :float4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/float4x4 [] :float4x4]
           [:func 'top.kzre.homunculus.backend.shader.dsl/texture2D     [] :texture2D]
           [:func 'top.kzre.homunculus.backend.shader.dsl/sampler-state [] :sampler]
           [:func 'top.kzre.homunculus.backend.shader.dsl/cbuffer       [] :cbuffer]
           ))






(defrecord HLSLFrontend []
  tp/IFrontendInfo
  (frontend-types [_]
    [:float :float2 :float3 :float4 :float4x4 :bool :int :texture2D :sampler :cbuffer])

  (literal->type [_ val]
    (cond
      (float? val)   (ty/make-tcon :float)
      (integer? val) (ty/make-tcon :int)
      (true? val)    (ty/make-tcon :bool)
      (false? val)   (ty/make-tcon :bool)
      (nil? val)     (ty/make-tcon :float)
      :else          (ty/make-tvar (gensym "lit"))))

  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (ty/make-tcon tag)
        (ty/make-tcon (keyword (name tag))))))

  ;; 返回 HLSL 内置函数表，融合通用 shader builtins 和 HLSL 特有函数
  (builtin-functions [_]
    (merge builtin/common-builtins
           {;; HLSL 特有内置函数
            'tex2D           (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float2) (ty/make-tcon :float4)))
            'tex2Dlod        (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float4) (ty/make-tcon :float4)))
            'texCUBE         (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float3) (ty/make-tcon :float4)))
            'clip            (ty/make-tfun (ty/make-tcon :float) nil)
            'discard         (ty/make-tfun nil nil)
            'ddx             (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'ddy             (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'fwidth          (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'lerp            (ty/make-tfun (ty/make-tcon :float) (ty/make-tfun (ty/make-tcon :float) (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))))}))

  (builtin-symbols [_] symbol-tables)
  )
