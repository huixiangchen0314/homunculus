(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。提供统一的资源/入口声明，通过元数据传递给后端。"
  (:refer-clojure :exclude [float int]))

;;; ========= 类型构造器 =========================================
;;; 命名与 HLSL 原始拼写一致：
;;;   标量 / 向量 / 矩阵 → 全小写
;;;   纹理 / 采样器 / 缓冲 → PascalCase

;; ═══════════════════════════════════════════
;; 标量
;; ═══════════════════════════════════════════
(defn bool  [] nil)
(defn int   [] nil)
(defn uint  [] nil)
(defn float [] nil)

;; ═══════════════════════════════════════════
;; 向量
;; ═══════════════════════════════════════════
(defn bool2  [] nil) (defn bool3  [] nil) (defn bool4  [] nil)
(defn int2   [] nil) (defn int3   [] nil) (defn int4   [] nil)
(defn uint2  [] nil) (defn uint3  [] nil) (defn uint4  [] nil)
(defn float2 [] nil) (defn float3 [] nil) (defn float4 [] nil)

;; ═══════════════════════════════════════════
;; 矩阵
;; ═══════════════════════════════════════════
(defn float2x2 [] nil) (defn float2x3 [] nil) (defn float2x4 [] nil)
(defn float3x2 [] nil) (defn float3x3 [] nil) (defn float3x4 [] nil)
(defn float4x2 [] nil) (defn float4x3 [] nil) (defn float4x4 [] nil)

;; ═══════════════════════════════════════════
;; 纹理
;; ═══════════════════════════════════════════
(defn Texture1D        [] nil)
(defn Texture1DArray   [] nil)
(defn Texture2D        [] nil)
(defn Texture2DArray   [] nil)
(defn Texture3D        [] nil)
(defn TextureCube      [] nil)
(defn TextureCubeArray [] nil)
(defn Texture2DMS      [] nil)
(defn Texture2DMSArray [] nil)

;; ═══════════════════════════════════════════
;; 采样器
;; ═══════════════════════════════════════════
(defn SamplerState           [] nil)
(defn SamplerComparisonState [] nil)

;; ═══════════════════════════════════════════
;; 缓冲
;; ═══════════════════════════════════════════
(defn cbuffer           [] nil)
(defn Buffer            [] nil)
(defn StructuredBuffer  [] nil)
(defn ByteAddressBuffer [] nil)


;; 类型构造器名 → 全限定符号
(def ^:private type-ctor-name->sym
  {'bool  `bool,  'int   `int,  'uint  `uint,  'float `float
   'bool2 `bool2, 'bool3 `bool3, 'bool4 `bool4
   'int2  `int2,  'int3  `int3,  'int4  `int4
   'uint2 `uint2, 'uint3 `uint3, 'uint4 `uint4
   'float2 `float2, 'float3 `float3, 'float4 `float4
   'float2x2 `float2x2, 'float2x3 `float2x3, 'float2x4 `float2x4
   'float3x2 `float3x2, 'float3x3 `float3x3, 'float3x4 `float3x4
   'float4x2 `float4x2, 'float4x3 `float4x3, 'float4x4 `float4x4
   'Texture1D        `Texture1D
   'Texture1DArray   `Texture1DArray
   'Texture2D        `Texture2D
   'Texture2DArray   `Texture2DArray
   'Texture3D        `Texture3D
   'TextureCube      `TextureCube
   'TextureCubeArray `TextureCubeArray
   'Texture2DMS      `Texture2DMS
   'Texture2DMSArray `Texture2DMSArray
   'SamplerState           `SamplerState
   'SamplerComparisonState `SamplerComparisonState
   'cbuffer           `cbuffer
   'Buffer            `Buffer
   'StructuredBuffer  `StructuredBuffer
   'ByteAddressBuffer `ByteAddressBuffer})

(defn- fully-qualified-ctor [ctor-sym]
  (or (get type-ctor-name->sym ctor-sym)
      (throw (ex-info (str "Unknown type constructor: " ctor-sym)
                      {:ctor ctor-sym}))))



(defmacro defshader [stage name param-vec & body]
  (let [meta-map (merge {:shader/stage stage :shader/entry? true}
                        (meta name)
                        (meta param-vec))
        fn-form (list* 'fn param-vec body)]
    `(def ~(vary-meta name merge meta-map) ~fn-form)))



(defmacro defuniform
  "定义全局 uniform 常量。示例：(defuniform worldViewProj float4x4)"
  [name type-ctor]
  `(def ~(vary-meta name assoc
                    :shader/uniform? true)
     (~(fully-qualified-ctor type-ctor))))

(defmacro defstatic
  "定义全局静态变量专用宏. eg. (defstatic accumColor (float4 0.0 0.0 0.0 0.0))"
  [name type-ctor]
  `(def ~(vary-meta name assoc
                    :shader/static-var? true) ~type-ctor))

(defmacro deftexture
  "定义纹理资源。"
  [name register-kw]
  `(def ~(vary-meta name assoc
                    :shader/resource? true
                    :shader/resource-kind :texture2D
                    :shader/texture-register register-kw)
     (Texture2D)))

(defmacro defsampler
  "定义采样器资源。"
  [name register-kw]
  `(def ~(vary-meta name assoc
                    :shader/resource? true
                    :shader/resource-kind :sampler
                    :shader/sampler-register register-kw)
     (SamplerState)))

(defmacro defcbuffer
  "定义 cbuffer 资源。成员以交替的符号+类型构造器给出，如 lightDir float3。
   同时为每个成员生成隐式 def，使类型系统可见。"
  [name register-kw & members]
  (let [pairs (partition 2 members)
        map-expr (into {} (map (fn [[sym type-ctor]]
                                 [(keyword sym) (fully-qualified-ctor type-ctor)])
                               pairs))
        member-defs (map (fn [[sym type-ctor]]
                           (let [full-ctor (fully-qualified-ctor type-ctor)]
                             `(def ~(vary-meta sym assoc
                                               :shader/cbuffer-member true
                                               :shader/ignore-emit? true)
                                (~full-ctor))))
                         pairs)]
    `(do
       (def ~(vary-meta name assoc
                        :shader/resource? true
                        :shader/resource-kind :cbuffer
                        :shader/cbuffer-register register-kw
                        :shader/cbuffer-members map-expr)
         (top.kzre.homunculus.backend.shader.dsl/cbuffer))
       ~@member-defs
       )))