(ns top.kzre.homunculus.backend.hlsl.utils
  "HLSL 专用工具：保留字、类型映射、内置函数对照。")

(def reserved-words
  "HLSL 保留字集合，用于自动转义冲突的变量名。"
  #{"if" "else" "while" "for" "return" "true" "false"
    "float" "half" "double" "int" "uint" "bool" "void"
    "float2" "float3" "float4" "float3x3" "float4x4"
    "int2" "int3" "int4" "uint2" "uint3" "uint4"
    "matrix" "vector" "sampler" "sampler1D" "sampler2D"
    "sampler3D" "samplerCube" "Texture1D" "Texture2D"
    "Texture3D" "TextureCube" "RWTexture1D" "RWTexture2D"
    "RWTexture3D" "Buffer" "RWBuffer" "StructuredBuffer"
    "RWStructuredBuffer" "ByteAddressBuffer" "RWByteAddressBuffer"
    "cbuffer" "tbuffer" "register" "packoffset"
    "in" "out" "inout" "uniform" "static" "const"
    "discard" "compile" "namespace" "technique" "pass"
    "pixelshader" "vertexshader" "geometryshader" "hullshader"
    "domainshader" "computeshader"})

(def type-map
  "IR 类型关键字 → HLSL 类型字符串。"
  {:int64   "int"
   :int32   "int"
   :int16   "int16_t"
   :uint64  "uint"
   :uint32  "uint"
   :float64 "double"
   :float32 "float"
   :float16 "half"
   :bool    "bool"
   :nil     "void"
   :vector  "float4"          ;; 默认向量类型，实际可能由泛型确定
   :float2  "float2"
   :float3  "float3"
   :float4  "float4"
   :float3x3 "float3x3"
   :float4x4 "float4x4"
   :string  "float" })

(def builtin-map
  "Clojure 操作名 → HLSL 内置函数/运算符。"
  {'+         {:op " + " :infix true}
   '-         {:op " - " :infix true}
   '*         {:op " * " :infix true}
   '/         {:op " / " :infix true}
   'inc       {:fn "inc"}
   'dec       {:fn "dec"}
   'zero?     {:fn "!" :prefix true}     ;; 实际可能需要生成 (x == 0)
   'pos?      {:fn ">" :infix true :second "0"}
   'neg?      {:fn "<" :infix true :second "0"}
   'abs       {:fn "abs"}
   'min       {:fn "min"}
   'max       {:fn "max"}
   'clamp     {:fn "clamp"}
   'lerp      {:fn "lerp"}
   'sqrt      {:fn "sqrt"}
   'pow       {:fn "pow"}
   'sin       {:fn "sin"}
   'cos       {:fn "cos"}
   'tan       {:fn "tan"}
   'asin      {:fn "asin"}
   'acos      {:fn "acos"}
   'atan      {:fn "atan"}
   'atan2     {:fn "atan2"}
   'floor     {:fn "floor"}
   'ceil      {:fn "ceil"}
   'round     {:fn "round"}
   'trunc     {:fn "trunc"}
   'fmod      {:fn "fmod"}
   'dot       {:fn "dot"}
   'cross     {:fn "cross"}
   'normalize {:fn "normalize"}
   'length    {:fn "length"}
   'distance  {:fn "distance"}
   'reflect   {:fn "reflect"}
   'refract   {:fn "refract"}
   'transpose {:fn "transpose"}
   'mul       {:fn "mul"}
   'tex2D     {:fn "tex2D" :sample true}
   'texCube   {:fn "texCUBE" :sample true}
   'saturate  {:fn "saturate"}
   'ddx       {:fn "ddx"}
   'ddy       {:fn "ddy"}
   'fwidth    {:fn "fwidth"}})