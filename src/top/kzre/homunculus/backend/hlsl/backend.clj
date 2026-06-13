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
    (str "while " test " { " body " }"))   ;; 去掉外层括号

  (shader-block [_ stmts]
    (str "{\n" (fmt/indent 1) (clojure.string/join (str ";\n" (fmt/indent 1)) stmts) ";\n}"))

  ;; ── 函数 ──


  (shader-return [_ expr]
    (str "return " expr ";"))

  ;; ── 入口与阶段 ──
  (shader-entry-point [this stage fn-name]
    (case stage
      :vertex   (str "struct VSInput {\n"
                     "    float4 position : POSITION;\n"
                     "};\n"
                     "struct VSOutput {\n"
                     "    float4 pos : SV_POSITION;\n"
                     "};\n"
                     "VSOutput main(VSInput input) {\n"
                     "    VSOutput output;\n"
                     "    output.pos = " fn-name "(input.position);\n"
                     "    return output;\n"
                     "}")
      :fragment (str "float4 main(float4 pos : SV_POSITION) : SV_TARGET {\n"
                     "    return " fn-name "(pos);\n"
                     "}")
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
    (str "(" (sp/shader-type this dst-ty) ")" expr))

  ;; 结构体定义
  (shader-struct-decl [_ name members]
    (let [member-strs (map (fn [m]
                             (str "    " (:type m) " " (:name m)
                                  (when (:semantic m) (str " : " (:semantic m))) ";"))
                           members)]
      (str "struct " name " {\n"
           (clojure.string/join "\n" member-strs) "\n"
           "};")))

  ;; 程序组合
  (shader-program [_ functions structs globals entry]
    (clojure.string/join "\n\n" (filter seq
                                        [ (clojure.string/join "\n" globals)
                                         (clojure.string/join "\n" structs)
                                         (clojure.string/join "\n" functions)
                                         entry ])))
  )