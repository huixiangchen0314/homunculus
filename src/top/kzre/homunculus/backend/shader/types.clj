(ns top.kzre.homunculus.backend.shader.types
  "着色器通用类型辅助。提供类型关键字到字符串的映射，供各后端使用。
   每个后端可以覆写或扩展此映射。")

(def default-type-map
  {:int       "int"
   :float     "float"
   :bool      "bool"
   :float2    "float2"
   :float3    "float3"
   :float4    "float4"
   :float4x4  "float4x4"
   :texture2D "Texture2D"
   :sampler   "SamplerState"
   :cbuffer   "cbuffer"})

(defn shader-type-str
  "根据 IR 类型关键字返回默认的着色器类型字符串。
   各后端可以直接使用此函数，或用自己的映射覆盖。"
  [type-kw]
  (get default-type-map type-kw (name type-kw)))