(ns top.kzre.homunculus.backend.unity.utils
  "Unity 专用工具：类型映射、内置函数对照。")

(def type-map
  {:int     "int"
   :float   "float"
   :bool    "bool"
   :float2  "float2"
   :float3  "float3"
   :float4  "float4"
   :float4x4 "float4x4"
   :string  "float"})

(def builtin-map
  "Clojure 操作名 → Unity HLSL 内置函数/运算符。"
  {'+         {:op " + " :infix true}
   '-         {:op " - " :infix true}
   '*         {:op " * " :infix true}
   '/         {:op " / " :infix true}
   '<         {:op " < " :infix true}
   '>         {:op " > " :infix true}
   '<=        {:op " <= " :infix true}
   '>=        {:op " >= " :infix true}
   '==        {:op " == " :infix true}
   '!=        {:op " != " :infix true}
   '!         {:fn "!" :prefix true}
   '&&        {:op " && " :infix true}
   '||        {:op " || " :infix true}
   'pow       {:fn "pow"}
   'not       {:fn "!" :prefix true}
   'abs       {:fn "abs"}
   'min       {:fn "min"}
   'max       {:fn "max"}
   'clamp     {:fn "clamp"}
   'lerp      {:fn "lerp"}
   'sqrt      {:fn "sqrt"}
   'saturate  {:fn "saturate"}
   'sin       {:fn "sin"}
   'cos       {:fn "cos"}
   'tan       {:fn "tan"}
   'asin      {:fn "asin"}
   'acos      {:fn "acos"}
   'atan      {:fn "atan"}
   'atan2     {:fn "atan2"}
   'exp       {:fn "exp"}
   'log       {:fn "log"}
   'log2      {:fn "log2"}
   'log10     {:fn "log10"}
   'floor     {:fn "floor"}
   'ceil      {:fn "ceil"}
   'round     {:fn "round"}
   'trunc     {:fn "trunc"}
   'frac      {:fn "frac"}
   'ddx       {:fn "ddx"}
   'ddy       {:fn "ddy"}
   'fwidth    {:fn "fwidth"}
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
   'sample    {:fn "tex2D" :sample true}   ;; 统一到 tex2D
   'float     {:fn "float"}
   'int       {:fn "int"}
   'bool      {:fn "bool"}
   'int2      {:fn "int2"}
   'int3      {:fn "int3"}
   'int4      {:fn "int4"}
   'bool2     {:fn "bool2"}
   'bool3     {:fn "bool3"}
   'bool4     {:fn "bool4"}})