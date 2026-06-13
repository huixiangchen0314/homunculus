(ns top.kzre.homunculus.backend.unity.backend
  "Unity ShaderLab 对 IShaderBackend 的最小实现。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.unity.utils :as utils]
    [top.kzre.homunculus.backend.shader.protocol :as sp]
    [top.kzre.homunculus.backend.util.format :as fmt]
    [top.kzre.homunculus.backend.util.naming :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defrecord UnityBackend []
  sp/IShaderBackend

  ;; ── 类型与字面量（完全同 HLSL）──
  (shader-type [_ ir-type]
    (let [name (:name ir-type)]
      (or (get utils/type-map name) "float")))

  (shader-literal [_ val]
    (cond
      (integer? val) (str val)
      (float? val)   (str val)
      (true? val)    "true"
      (false? val)   "false"
      (nil? val)     "0.0"
      :else (throw (ex-info (str "Unity unsupported literal: " val) {:val val}))))

  ;; ── 变量（同 HLSL）──
  (shader-var-decl [this name ir-type mutable? init-expr]
    (let [type-str (if ir-type (sp/shader-type this ir-type) "float")
          var-name (n/safe-name name)
          decl     (if mutable? (str type-str " " var-name) (str "const " type-str " " var-name))]
      (if init-expr
        (str decl " = " init-expr ";")
        (str decl ";"))))

  (shader-var-ref [this name]
    (if (get utils/builtin-map (symbol name))
      name
      (n/safe-name name)))

  (shader-assign [_ var val]
    (str var " = " val ";"))

  ;; ── 控制流（同 HLSL）──
  (shader-if [_ test then else]
    (if else
      (str "if (" test ") { " then " } else { " else " }")
      (str "if (" test ") { " then " }")))

  (shader-while [_ test body]
    (str "while (" test ") { " body " }"))

  (shader-block [_ stmts]
    (str "{\n" (fmt/indent 1) (str/join (str ";\n" (fmt/indent 1)) stmts) ";\n}"))

  ;; ── 函数（同 HLSL）──
  (shader-function-decl [this name params return-type body]
    (str return-type " " (n/safe-name name) "(" (fmt/comma-sep params) ")\n"
         (sp/shader-block this [body])))

  (shader-return [_ expr]
    (str "return " expr ";"))

  ;; ── 函数调用（Unity 使用 tex2D 采样）──
  (shader-call [this fn-name args]
    (let [sym (symbol fn-name)
          info (get utils/builtin-map sym)]
      (if info
        (cond
          (:infix info)
          (if (= 2 (count args))
            (str "(" (first args) (:op info) (second args) ")")
            (throw (ex-info "Infix op requires 2 args" {:fn fn-name :args args})))

          (:sample info)   ;; Unity 使用 tex2D 函数
          (let [[tex sam coord] args]   ; 忽略额外参数，只取前三个
            (str "tex2D(" tex ", " coord ")"))   ;; 采样器通过 tex 自动关联

          (:fn info)
          (str (:fn info) "(" (str/join ", " args) ")")

          :else
          (str fn-name "(" (str/join ", " args) ")"))
        (str fn-name "(" (str/join ", " args) ")"))))

  (shader-cast [this expr src-ty dst-ty]
    (str "(" (sp/shader-type this dst-ty) ")" expr))

  (shader-struct-decl [_ name members]
    (let [member-strs (map (fn [m]
                             (str "    " (:type m) " " (:name m)
                                  (when (:semantic m) (str " : " (:semantic m))) ";"))
                           members)]
      (str "struct " name " {\n"
           (str/join "\n" member-strs) "\n"
           "};")))

  ;; ── 程序组合：生成完整 ShaderLab 代码 ──
  (shader-program [this functions structs globals entry-specs]
    (let [hlsl-code (str/join "\n" (filter seq [globals structs functions]))
          pragmas   (map (fn [{:keys [stage fn-name]}]
                           (case stage
                             :vertex   (str "#pragma vertex " (n/safe-name fn-name))
                             :fragment (str "#pragma fragment " (n/safe-name fn-name))
                             (str "#pragma fragment " (n/safe-name fn-name))))
                         entry-specs)
          shader-str (str
                       "Shader \"Custom/" (or (:fn-name (first entry-specs)) "main") "\" {\n"
                       "    SubShader {\n"
                       "        Pass {\n"
                       "            HLSLPROGRAM\n"
                       "            " (str/join "\n            " pragmas) "\n"
                       "            " hlsl-code "\n"
                       "            ENDHLSL\n"
                       "        }\n"
                       "    }\n"
                       "}")]
      shader-str))

  ;; ── 资源声明：Unity 使用 sampler2D 或 Texture2D + SamplerState ──
  (shader-resource-decl [_ name res-type args]
    ;; 简单起见，生成 uniform Texture2D + SamplerState 组合
    (case res-type
      :texture2D (str "uniform Texture2D<float4> " name ";")
      :sampler   (str "uniform SamplerState " name ";")
      :cbuffer   (str "cbuffer " name " { ... };")  ;; 简化
      (throw (ex-info "Unknown resource type" {:type res-type})))))