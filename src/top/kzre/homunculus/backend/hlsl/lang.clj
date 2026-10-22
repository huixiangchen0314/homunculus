(ns top.kzre.homunculus.backend.hlsl.lang
  (:require
    [top.kzre.homunculus.core.error :as err]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

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

(defn sampler-type-str [compare?]
  (if compare?
    "SamplerComparisonState"
    "SamplerState"))

(def ^:private sample-fns
  "纹理采样函数。这些调用在 GLSL 降级时翻译为 texture() / textureLod() /
   textureGrad() / textureGather() 等——均需显式 sampler 参数。

   u/i 前缀的变体在 HLSL 里映射到同样的方法名（Sample / SampleLevel / ...），
   但它们的 tex 参数是 uTexture* / iTexture*，需要 sampler（SamplerState），
   因此仍归入 sample-fns。

   不含 load / uload / iload（texelFetch 路径，不走 sampler）和 textureSize
   （尺寸查询，也不走 sampler）。这两类由降级器另行处理。"
  #{;; float 变体
    'sample 'sampleLod 'sampleBias 'sampleCmp 'sampleCmpLevelZero 'sampleGrad
    'gather 'gatherRed 'gatherGreen 'gatherBlue 'gatherAlpha
    ;; uint 变体
    'usample 'usampleLod 'usampleBias 'usampleGrad
    'ugather 'ugatherRed 'ugatherGreen 'ugatherBlue 'ugatherAlpha
    ;; int 变体
    'isample 'isampleLod 'isampleBias 'isampleGrad
    'igather 'igatherRed 'igatherGreen 'igatherBlue 'igatherAlpha})

(defn sample-fn?
  "判断函数名是否为纹理采样函数。fn-name 是符号。

   这些是 HLSL 中通过 sampler 对象访问纹理的函数——与 load
   （texelFetch 路径）和 textureSize（尺寸查询）区分开。"
  [fn-name]
  (contains? sample-fns fn-name))

(def ^:private texel-fetch-fn-map
  "texel fetch 函数 → GLSL 对应函数名。

   这些调用不需要 sampler——按整数坐标直接读取 texel 值，
   走 GLSL 的 texelFetch 路径。与 sample-fns（走 texture/textureLod/
   textureGrad/textureGather，需要 sampler）相对。

   不带 textureSize——尺寸查询也是\"无 sampler\"路径，但它是查询
   而非 fetch，语义不同，本 map 不涵盖。textureSize 由降级器单独
   处理（对应 GLSL 的 textureSize）。

   降级器用此表决定：命中的调用生成 texelFetch(tex, intCoord, lod)，
   未命中的走 sample-fns 的 texture() 路径。"
  {'load  'texelFetch
   'uload 'texelFetch
   'iload 'texelFetch})

(defn texel-fetch-fn?
  "判断函数名是否为 texel fetch 函数。fn-name 是符号。

   这些是 HLSL 中 .Load(...) 路径的函数——不经过 sampler，
   按整数坐标直接取 texel。与 sample-fn? 判断的采样函数
   （.Sample(...)，需要 SamplerState）相对。"
  [fn-name]
  (contains? texel-fetch-fn-map fn-name))

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