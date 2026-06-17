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
   ir-type 可以是关键字（如 :float4）或 IType 对象（从 types.type 获取 type-sym 后的结果）。
   若为 nil 或未知类型，返回 \"void\"。"
  [type-kw]
  (if type-kw
    (get default-type-map type-kw (name type-kw))
    "void"))