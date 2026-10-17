(ns top.kzre.homunculus.backend.glsl.lang
  (:require
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ═══════════════════════════════════════════
;; 类型映射
;; ═══════════════════════════════════════════

(def ^:private scalar-map
  {'bool "bool" 'int "int" 'uint "uint" 'float "float"})

(def ^:private vector-map
  {'bool2 "bvec2" 'bool3 "bvec3" 'bool4 "bvec4"
   'int2  "ivec2" 'int3  "ivec3" 'int4  "ivec4"
   'uint2 "uvec2" 'uint3 "uvec3" 'uint4 "uvec4"
   'float2 "vec2" 'float3 "vec3" 'float4 "vec4"})

(def ^:private matrix-map
  {'float2x2 "mat2"   'float2x3 "mat2x3" 'float2x4 "mat2x4"
   'float3x2 "mat3x2" 'float3x3 "mat3"   'float3x4 "mat3x4"
   'float4x2 "mat4x2" 'float4x3 "mat4x3" 'float4x4 "mat4"})

(defn type->str
  "IR 类型 → GLSL 类型字符串。仅处理 TCon。"
  [ir-type]
  (if-let [sym (ty/type-sym ir-type)]
    (or (scalar-map sym)
        (vector-map sym)
        (matrix-map sym)
        (name sym))
    (throw
      (ex-info "unexpected non-TCon type at GLSL emit"
               {:type ir-type}))))

(defn literal->str
  "字面量值 → GLSL 字面量字符串。float 常量无需 f 后缀。"
  [val]
  (cond
    (nil? val)     "void"
    (integer? val) (str val)
    (float? val)   (let [s (str val)]
                     (if (or (re-find #"\." s) (re-find #"[eE]" s))
                       s
                       (str s ".0")))
    (true? val)    "true"
    (false? val)   "false"
    :else
    (throw
      (ex-info "unsupported literal at GLSL emit" {:val val}))))

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
  "判断 (stage, semantic) 是否对应 GLSL 内置变量。

   vertex   :position → gl_Position
   fragment :depth    → gl_FragDepth"
  [stage semantic]
  (boolean
    (or (and (= stage :vertex)   (= semantic :position))
        (and (= stage :fragment) (= semantic :depth)))))

(defn builtin-var-name
  "返回 (stage, semantic) 对应的 GLSL 内置变量名。
   非内置语义抛异常。"
  [stage semantic]
  (cond
    (and (= stage :vertex)   (= semantic :position))
    "gl_Position"

    (and (= stage :fragment) (= semantic :depth))
    "gl_FragDepth"

    :else
    (throw (ex-info "not a GLSL builtin semantic"
                    {:stage stage :semantic semantic}))))
