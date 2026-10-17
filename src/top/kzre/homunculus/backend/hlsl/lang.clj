(ns top.kzre.homunculus.backend.hlsl.lang
  (:require
   [top.kzre.homunculus.backend.format :refer [T]]
   [top.kzre.homunculus.core.error :as err]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn type->str [ir-type]
  (cond
    (ty/vec-type? ir-type)
    (let [elem-type (type->str (ty/vec-element-type ir-type))
          size (ty/value-val (ty/vec-size ir-type))]
      (T "${elem-type}[${size}]"))
    (ty/type-sym ir-type) (name (ty/type-sym ir-type))
    ;:else (throw (ex-info (str "Unknown type: " ir-type) {}))
    :else (pr-str ir-type)
    ))

(defn literal->str [val]
  (cond
    (nil? val)   nil
    (integer? val) (str val)
    (float? val)   (str val)
    (true? val)    "true"
    (false? val)   "false"
    :else (throw
            (err/compile-error
              :unsupported-literal-type
              (str "Unsupported literal type for value: " (pr-str val))))))

(defn sematic->str [kw stage]
  (case kw
    :position (if (= stage :vertex) "POSITION" "SV_POSITION")
    :normal       "NORMAL"
    :tangent      "TANGENT"
    :texcoord0    "TEXCOORD0"
    :texcoord1    "TEXCOORD1"
    :texcoord2    "TEXCOORD2"
    :texcoord3    "TEXCOORD3"
    :texcoord4    "TEXCOORD4"
    :texcoord5    "TEXCOORD5"
    :texcoord6    "TEXCOORD6"
    :texcoord7    "TEXCOORD7"
    :color0       "COLOR0"
    :color1       "COLOR1"
    :target0      "SV_TARGET"
    :target1      "SV_TARGET1"
    :depth        "SV_DEPTH"
    :instance-id  "SV_INSTANCEID"
    :vertex-id    "SV_VERTEXID"
    :primitive-id "SV_PRIMITIVEID"
    :user0        "USER0"
    :user1        "USER1"
    :user2        "USER2"
    :user3        "USER3"
    (throw (err/compile-error
             :unknown-hlsl-sematic
             (str "Unknow HLSL sematic: " kw)
             ))))

(def ^:private texture-dimension-str-map*
  {:1d          "Texture1D"
   :2d          "Texture2D"
   :3d          "Texture3D"
   :cube        "TextureCube"
   :1d-array    "Texture1DArray"
   :2d-array    "Texture2DArray"
   :cube-array  "TextureCubeArray"
   :2d-ms       "Texture2DMS"
   :2d-ms-array "Texture2DMSArray"})

(defn texture-dimension->type-str [dim]
  (get texture-dimension-str-map* dim))

(defn sampler-type-str [compare?]
  (if compare?
    "SamplerComparisonState"
    "SamplerState"))

(def ^:private sample-fns
  "纹理采样函数。这些调用在 GLSL 降级时翻译为 texture() / textureLod() /
   textureGrad() / textureGather() 等——均需显式 sampler 参数。

   不含 load（texelFetch，不走 sampler）和 textureSize（尺寸查询，
   也不走 sampler）。这两类由降级器另行处理。"
  #{'sample 'sampleLod 'sampleBias 'sampleCmp 'sampleCmpLevelZero 'sampleGrad
    'gather 'gatherRed 'gatherGreen 'gatherBlue 'gatherAlpha})

(defn sample-fn?
  "判断函数名是否为纹理采样函数。fn-name 是符号。

   这些是 HLSL 中通过 sampler 对象访问纹理的函数——与 load
   （texelFetch 路径）和 textureSize（尺寸查询）区分开。"
  [fn-name]
  (contains? sample-fns fn-name))

(def ^:private builtin-types
  "HLSL 内置类型集合。struct-type? 的反面——不在这个集合里的 TCon 是用户 struct。"
  '#{bool int uint float
     bool2 bool3 bool4
     int2 int3 int4
     uint2 uint3 uint4
     float2 float3 float4
     float2x2 float2x3 float2x4
     float3x2 float3x3 float3x4
     float4x2 float4x3 float4x4
     Texture1D Texture2D Texture3D TextureCube
     Texture1DArray Texture2DArray TextureCubeArray
     Texture2DMS Texture2DMSArray
     SamplerState SamplerComparisonState
     Buffer StructuredBuffer ByteAddressBuffer
     cbuffer})

(defn builtin-type?
  "判断是否为 HLSL 内置类型。

   type-sym 是符号——如 'float4 / 'Texture2D / 'MyStruct。
   不在内置集合里的符号视为用户 struct。"
  [type-sym]
  (contains? builtin-types type-sym))


(def ^:private texture-dimension->symbol-map
  {:1d           'Texture1D
   :1d-array     'Texture1DArray
   :2d           'Texture2D
   :2d-array     'Texture2DArray
   :3d           'Texture3D
   :cube         'TextureCube
   :cube-array   'TextureCubeArray
   :2d-ms        'Texture2DMS
   :2d-ms-array  'Texture2DMSArray
   :buffer       'Buffer})

(defn texture-dimension->type-sym
  "纹理维度关键字 → HLSL 纹理类型符号。
   :2d → 'Texture2D，:2d-array → 'Texture2DArray，:buffer → 'Buffer。

   未知维度抛异常。"
  [dim]
  (or (get texture-dimension->symbol-map dim)
      (throw (err/compile-error
               :unknown-texture-dimension
               (str "Unknown texture dimension: " dim)))))


(defn struct-type?
  "判断类型是否为给定 struct 名称集合中的成员。

   本函数从 HLSL 的视角判断——用户定义的 struct 是那些在
   shader 里通过 :struct 节点声明过的类型。struct-names 是
   从 asts 收集的已定义 struct 名称集合。

   struct-names —— 已定义的 struct 名称集合（符号集合）。"
  [ir-type struct-names]
  (let [sym (ty/type-sym ir-type)]
    (and (some? sym)
         (contains? struct-names sym))))