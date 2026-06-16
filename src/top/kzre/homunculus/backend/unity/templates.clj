;; top.kzre.homunculus.backend.unity.templates
(ns top.kzre.homunculus.backend.unity.templates
  "Unity ShaderLab 模板。提供 ShaderLab 外层结构与 Unity 特定类型映射。"
  (:require
   [clojure.string :as str]
   [top.kzre.homunculus.backend.shader.types :as st]
   [top.kzre.homunculus.backend.util.format :refer [T]]
   [top.kzre.homunculus.core.types.type :as t]))

;; ── ShaderLab 骨架 ──────────────────────
(defn shader [name body]
  (T "Shader \"${name}\" {\n${body}\n}"))

(defn properties [& prop-lines]
  (str "Properties {\n" (str/join "\n" prop-lines) "\n}"))

(defn property-float [name display default]
  (T "${name} (\"${display}\", Float) = ${default}"))

(defn property-vector [name display default]
  (T "${name} (\"${display}\", Vector) = (${default})"))

(defn property-color [name display default]
  (T "${name} (\"${display}\", Color) = (${default})"))

(defn property-2d [name display default-tex]
  (T "${name} (\"${display}\", 2D) = \"${default-tex}\" {}"))

(defn property-cube [name display default-tex]
  (T "${name} (\"${display}\", Cube) = \"${default-tex}\" {}"))

(defn subshader [tags body]
  (str "SubShader {\n" (when tags (str tags "\n")) body "\n}"))

(defn pass [name tags body]
  (T "Pass {\nName \"${name}\"\n${tags}\n${body}\n}"))

(defn pragma-vertex [name]
  (T "#pragma vertex ${name}"))
(defn pragma-fragment [name]
  (T "#pragma fragment ${name}"))
(defn include-unity-cg [] "#include \"UnityCG.cginc\"")

(defn hlsl-program [body]
  (T "HLSLPROGRAM\n${body}\nENDHLSL"))

;; ── 类型映射（Unity 支持 half/fixed） ──
(def unity-type-map
  (merge st/default-type-map
         {:half      "half"
          :half2     "half2"
          :half3     "half3"
          :half4     "half4"
          :fixed     "fixed"
          :fixed2    "fixed2"
          :fixed3    "fixed3"
          :fixed4    "fixed4"
          :sampler2D "sampler2D"
          :samplerCUBE "samplerCUBE"
          :texture2D "Texture2D"
          :texture3D "Texture3D"}))

(defn unity-type-str [ir-type]
  (get unity-type-map (t/type-sym ir-type)
       (name (t/type-sym ir-type))))