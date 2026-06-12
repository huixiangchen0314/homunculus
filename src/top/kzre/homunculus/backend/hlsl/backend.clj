(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 对 IShaderBackend 的实现。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.backend.hlsl.utils :as utils]
            [top.kzre.homunculus.backend.util.naming :as n]
            [top.kzre.homunculus.backend.util.format :as fmt]))

(defrecord HLSLBackend []
  sp/IShaderBackend

  ;; ── 类型与字面量 ──
  ;; backend/hlsl/backend.clj

  (shader-type [_ ir-type]
    (let [name (:name ir-type)
          _ (println "DEBUG shader-type name:" name)   ;; 调试输出
          type-str (or (get utils/type-map name)
                       (do (println "WARNING: Unknown HLSL type, defaulting to float:" name)
                           "float"))]
      type-str))

  (shader-literal [_ val]
    (cond
      (integer? val) (str val)
      (float? val)   (str val)
      (true? val)    "true"
      (false? val)   "false"
      :else (throw (ex-info (str "HLSL unsupported literal: " val) {:val val}))))

  ;; ── 变量 ──
  (shader-var-decl [this name ir-type mutable? init-expr]
    (let [type-str (if ir-type (sp/shader-type this ir-type) "float")  ;; 默认 float
          var-name (n/safe-name name)
          decl     (if mutable? (str type-str " " var-name) (str "const " type-str " " var-name))]
      (if init-expr
        (str decl " = " init-expr ";")
        (str decl ";"))))
  (shader-function-decl [this name params return-type body]
    (str return-type " " (n/safe-name name) "(" (fmt/comma-sep params) ")\n"
         (sp/shader-block this [body])))

  (shader-var-ref [_ name]
    (n/safe-name name))

  ;; ── 赋值 ──
  (shader-assign [_ var val]
    (str var " = " val ";"))

  ;; ── 控制流 ──
  (shader-if [_ test then else]
    (if else
      (str "if (" test ") { " then " } else { " else " }")
      (str "if (" test ") { " then " }")))

  (shader-while [_ test body]
    (str "while (" test ") { " body " }"))

  (shader-block [_ stmts]
    (str "{\n" (fmt/indent 1) (clojure.string/join (str ";\n" (fmt/indent 1)) stmts) ";\n}"))

  ;; ── 函数 ──


  (shader-return [_ expr]
    (str "return " expr ";"))

  ;; ── 入口与阶段 ──
  (shader-entry-point [_ stage fn-name]
    ;; 简化：根据 stage 关键字生成系统语义，这里仅示例
    (case stage
      :vertex   (str "void main() { " fn-name "(); }")  ;; 实际需附加 SV_Position 等，由 codegen 处理
      :fragment (str "void main() { " fn-name "(); }")
      (throw (ex-info "Unknown shader stage" {:stage stage}))))

  ;; ── 类型转换 ──
  (shader-cast [this expr src-ty dst-ty]
    (str "(" (sp/shader-type this dst-ty) ")" expr)))