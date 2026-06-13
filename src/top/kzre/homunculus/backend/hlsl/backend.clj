(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 对 IShaderBackend 的实现。"
  (:require
   [clojure.string :as str]
   [top.kzre.homunculus.backend.hlsl.utils :as utils]
   [top.kzre.homunculus.backend.shader.protocol :as sp]
   [top.kzre.homunculus.backend.util.format :as fmt]
   [top.kzre.homunculus.backend.util.naming :as n]))

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
      ;; TODO recur-elim 支持默认值推导后删除
      (nil? val) "0.0"
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

  (shader-var-ref [this name]
    (if (get utils/builtin-map (symbol name))
      name
      (n/safe-name name)))

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
  (shader-entry-point [this stage fn-name]
    ;; 从某个 define 节点中读取阶段信息，但此方法只接收 stage 参数，因此需要外部传入正确的 stage
    ;; 我们将在 generate 函数中传入正确的 stage
    (case stage
      :vertex   (str "struct VSOutput { float4 pos : SV_POSITION; };\n"
                     "VSOutput main() {\n"
                     "    VSOutput o;\n"
                     "    o.pos = " fn-name "();\n"
                     "    return o;\n}")
      :fragment (str "float4 main() : SV_TARGET {\n"
                     "    return " fn-name "();\n}")
      ;; 默认
      (str "void main() { " fn-name "(); }")))

  (shader-call [this fn-name args]
    (let [sym (symbol fn-name)  ;; 变量引用返回的是字符串，需转符号以匹配 builtin-map
          info (get utils/builtin-map sym)]
      (if info
        (cond
          (:infix info)
          (if (= 2 (count args))
            (str "(" (first args) (:op info) (second args) ")")
            (throw (ex-info "Infix op requires 2 args" {:fn fn-name :args args})))

          (:fn info)
          (str (:fn info) "(" (str/join ", " args) ")")

          :else
          (str fn-name "(" (str/join ", " args) ")"))
        ;; 未找到内置映射，按用户函数处理
        (str fn-name "(" (str/join ", " args) ")"))))

  ;; ── 类型转换 ──
  (shader-cast [this expr src-ty dst-ty]
    (str "(" (sp/shader-type this dst-ty) ")" expr)))