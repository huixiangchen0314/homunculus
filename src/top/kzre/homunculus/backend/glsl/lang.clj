(ns top.kzre.homunculus.backend.glsl.lang
  (:require
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.backend.clang.lang :as clang.lang]))


;; ═══════════════════════════════════════════
;; 类型映射
;; ═══════════════════════════════════════════

(def ^:private scalar-map
  {"bool" "bool" "int" "int" "uint" "uint" "float" "float"})

(def ^:private vector-map
  {"bool2" "bvec2" "bool3" "bvec3" "bool4" "bvec4"
   "int2"  "ivec2" "int3"  "ivec3" "int4"  "ivec4"
   "uint2" "uvec2" "uint3" "uvec3" "uint4" "uvec4"
   "float2" "vec2" "float3" "vec3" "float4" "vec4"})

(def ^:private matrix-map
  {"float2x2" "mat2"   "float2x3" "mat2x3" "float2x4" "mat2x4"
   "float3x2" "mat3x2" "float3x3" "mat3"   "float3x4" "mat3x4"
   "float4x2" "mat4x2" "float4x3" "mat4x3" "float4x4" "mat4"})

(defn type->str
  "IR 类型 → GLSL 类型字符串。

   TVec —— 由 clang.lang/type->str 翻译为 element[size] 形式。
   TCon —— 先转字符串，再查 GLSL 映射表；未命中时原样返回
           （用户 struct 名、sampler2D 等）。
   其他 —— clang.lang/type->str 的 strict 模式抛异常。"
  [ir-type]
  (let [type-str (clang.lang/type->str ir-type)]
    (or (scalar-map type-str)
        (vector-map type-str)
        (matrix-map type-str)
        type-str)))

(defn naming-fn
  "GLSL 的函数名/操作符映射。返回符号。

   接受符号或字符串——先统一为字符串再查表：
     ▸ 操作符（+ - * / 等）→ C 风格操作符符号
     ▸ HLSL 类型构造（float4 / float3 等）→ GLSL 类型符号
     ▸ 其他内置函数（dot / normalize / texture 等）→ 原样符号

   非符号、非字符串 —— 返回 nil。"
  [op]
  (let [s (cond
            (symbol? op) (name op)
            (string? op) op
            :else        nil)]
    (when s
      (or (scalar-map s)
          (vector-map s)
          (matrix-map s)
          s))))

(def ^:private sample-fn-map
  {"sample "            "texture"
   "sampleLod"          "textureLod"
   "sampleBias"         "texture"
   "sampleCmp"          "texture"
   "sampleCmpLevelZero" "textureLod"
   "sampleGrad "        "textureGrad"
   "gather"             "textureGather"
   "gatherRed"          "textureGather"
   "gatherGreen"        "textureGather"
   "gatherBlue"         "textureGather"
   "gatherAlpha"        "textureGather"})

(defn sample-fn-str [fn-name]
  (get sample-fn-map (name fn-name)))

(def ^:private sample-grater-comp-map
  {"gather"      nil
   "gatherRed"   0
   "gatherGreen" 1
   "gatherBlue"  2
   "gatherAlpha" 3})

(defn sample-grater-component [fn-name]
  (get sample-grater-comp-map (name fn-name)))

;; ═══════════════════════════════════════════
;; sampler 类型推导
;; ═══════════════════════════════════════════

(defn- element-prefix
  [element-type]
  (let [sym (ty/type-sym element-type)
        n   (when sym (name sym))]
    (cond
      (and n (.startsWith n "uint")) "usampler"
      (and n (.startsWith n "int"))  "isampler"
      :else                          "sampler")))

(defn- dimension-suffix
  [dimension compare?]
  (if compare?
    (case dimension
      :1d       "1DShadow"
      :2d       "2DShadow"
      :cube     "CubeShadow"
      :1d-array "1DArrayShadow"
      :2d-array "2DArrayShadow"
      (throw (ex-info "unsupported shadow sampler dimension"
                      {:dimension dimension})))
    (case dimension
      :1d          "1D"
      :2d          "2D"
      :3d          "3D"
      :cube        "Cube"
      :1d-array    "1DArray"
      :2d-array    "2DArray"
      :cube-array  "CubeArray"
      :2d-ms       "2DMS"
      :2d-ms-array "2DMSArray"
      :buffer      "Buffer"
      (throw (ex-info "unsupported sampler dimension"
                      {:dimension dimension})))))

(defn sampler-type-name
  "根据纹理 dimension、元素类型、sampler compare? 推导 GLSL
   sampler 类型名字符串。"
  [dimension element-type compare?]
  (str (element-prefix element-type)
       (dimension-suffix dimension compare?)))

(defn sampler-glsl-name
  "生成 sampler uniform 的 GLSL 变量名。
   规则：<texture-name>_<sampler-name>。"
  [texture-name sampler-name]
  (str texture-name "_" sampler-name))

;; ═══════════════════════════════════════════
;; 语言规则
;; ═══════════════════════════════════════════

(defn flat-required?
  "整数 / 无符号整数的插值必须 flat。"
  [ir-type]
  (let [sym (ty/type-sym ir-type)
        n   (when sym (name sym))]
    (boolean
      (or (and n (.startsWith n "int"))
          (and n (.startsWith n "uint"))))))


;; ═══════════════════════════════════════════
;; 语义 → GLSL 内置变量
;; ═══════════════════════════════════════════


(defn builtin-semantic?
  [stage semantic]
  (boolean
    (or (and (= stage :vertex)   (= semantic :position))
        (and (= stage :fragment) (= semantic :position))
        (and (= stage :fragment) (= semantic :depth)))))

(defn builtin-var-name
  [stage semantic]
  (cond
    (and (= stage :vertex)   (= semantic :position))
    "gl_Position"
    (and (= stage :fragment) (= semantic :position))
    "gl_FragCoord"
    (and (= stage :fragment) (= semantic :depth))
    "gl_FragDepth"
    :else
    (throw (ex-info "not a GLSL builtin semantic"
                    {:stage stage :semantic semantic}))))
