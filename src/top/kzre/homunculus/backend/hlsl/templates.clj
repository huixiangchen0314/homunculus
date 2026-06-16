(ns top.kzre.homunculus.backend.hlsl.templates
  "HLSL 代码模板。使用 T 宏生成语法片段，纯字符串变换。"
  (:require [top.kzre.homunculus.backend.util.format :refer [T]]))

;; ── 类型映射 ─────────────────────────────
(defn hlsl-type
  "将 IR 类型关键字映射为 HLSL 类型字符串。"
  [ir-type-name]
  (case ir-type-name
    :int       "int"
    :int32     "int"
    :int64     "int"
    :float     "float"
    :float32   "float"
    :float64   "float"
    :bool      "bool"
    :float2    "float2"
    :float3    "float3"
    :float4    "float4"
    :float4x4  "float4x4"
    :texture2D "Texture2D"
    :sampler   "SamplerState"
    :cbuffer   "cbuffer"
    ;; 默认回退
    (name ir-type-name)))

;; ── 字面量 ──────────────────────────────
(defn hlsl-literal
  "Clojure 值 -> HLSL 字面量字符串。"
  [val]
  (cond
    (integer? val) (str val)
    (float? val)   (str val)
    (true? val)    "true"
    (false? val)   "false"
    (nil? val)     "0"
    :else (pr-str val)))

;; ── 变量声明与引用 ──────────────────────
(defn var-decl
  "变量声明：float3 position;"
  [type-str name]
  (T "${type-str} ${name};"))

(defn var-decl-init
  "带初始值的变量声明：float3 color = float3(1,0,0);"
  [type-str name init-str]
  (T "${type-str} ${name} = ${init-str};"))

(defn var-ref
  "变量引用，直接返回名称。"
  [name]
  name)

;; ── 赋值 ────────────────────────────────
(defn assign
  "赋值语句：target = value;"
  [target-str value-str]
  (T "${target-str} = ${value-str};"))

;; ── 函数调用 ─────────────────────────────
(defn call
  "函数调用或运算符：fnName(arg1, arg2)。"
  [fn-name args-str]
  (T "${fn-name}(${args-str})"))

;; ── 类型转换 ─────────────────────────────
(defn type-cast   ;; 重命名后的显式类型转换
  "显式类型转换：(float)expr"
  [type-str expr-str]
  (T "(${type-str})${expr-str}"))

;; ── 成员访问 ─────────────────────────────
(defn member-access
  "对象成员访问：obj.member"
  [target-str member]
  (let [member-str (if (keyword? member) (name member) member)]
    (T "${target-str}.${member-str}")))

;; ── 控制流 ──────────────────────────────
(defn if-stmt
  "if 语句：if (cond) { body }，无 else。"
  [condition body]
  (T "if (${condition}) { ${body} }"))

(defn if-else-stmt
  "if-else 语句：if (cond) { then } else { else }"
  [condition then-body else-body]
  (T "if (${condition}) { ${then-body} } else { ${else-body} }"))

(defn while-stmt
  "while 循环：while (cond) { body }"
  [condition body]
  (T "while (${condition}) { ${body} }"))

(defn for-stmt
  "for 循环：for(init; cond; iter) { body }"
  [init-str cond-str iter-str body]
  (T "for(${init-str}; ${cond-str}; ${iter-str}) { ${body} }"))

;; ── 函数定义 ─────────────────────────────
(defn func-signature
  "函数签名：float4 main(VSInput input)"
  [return-type name params-str]
  (T "${return-type} ${name}(${params-str})"))

(defn func-signature-semantic
  "带语义的函数签名：float4 main(VSInput input) : SV_POSITION"
  [return-type name params-str semantic]
  (T "${return-type} ${name}(${params-str}) : ${semantic}"))

(defn func-body
  "函数体（带大括号）：{ body }"
  [body-str]
  (T "{ ${body-str} }"))

(defn return-stmt
  "return 语句：return expr;"
  [expr-str]
  (T "return ${expr-str};"))

;; ── 结构体 ───────────────────────────────
(defn struct-decl
  "结构体声明：struct Name { members };"
  [name members-str]
  (T "struct ${name} { ${members-str} };"))

(defn struct-member
  "结构体成员：float3 position : SV_POSITION;"
  [type-str name semantic]
  (if semantic
    (T "${type-str} ${name} : ${semantic};")
    (T "${type-str} ${name};")))

;; ── 资源声明 ─────────────────────────────
(defn texture2d-decl
  "纹理声明：Texture2D name : register(t0);"
  [name register-slot]
  (T "Texture2D ${name} : register(t${register-slot});"))

(defn sampler-decl
  "采样器声明：SamplerState name : register(s0);"
  [name register-slot]
  (T "SamplerState ${name} : register(s${register-slot});"))

(defn cbuffer-decl
  "cbuffer 声明：cbuffer Name : register(b0) { members };"
  [name register-slot members-str]
  (T "cbuffer ${name} : register(b${register-slot}) { ${members-str} };"))

;; ── 入口包装 ─────────────────────────────
(defn entry-input-struct
  "生成顶点/片段着色器的输入结构体定义。"
  [name members-str]
  (struct-decl name members-str))

(defn entry-output-struct
  "生成输出结构体定义。"
  [name members-str]
  (struct-decl name members-str))

(defn entry-wrapper
  "生成完整的入口函数（如 main），负责从输入拷贝到输出，并调用核心函数。
   此处只提供外壳，内部逻辑由调用者拼接。"
  [stage func-name input-type output-type body-str]
  (case stage
    :vertex   (T "VSOutput ${func-name}(VSInput input) { ${body-str} }")
    :fragment (T "float4 ${func-name}(VSOutput input) : SV_TARGET { ${body-str} }")
    ;; 默认使用通用签名
    (T "void ${func-name}() { ${body-str} }")))