(ns top.kzre.homunculus.backend.hlsl.templates
  "HLSL 代码模板。使用 T 宏生成语法片段，纯字符串变换。
   所有模板函数均使用 & keys 解构参数，提高可读性。"
  (:refer-clojure :exclude [import])
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.util.format :refer [T]]))

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

;; ── 变量声明 ────────────────────────────
(defn var-decl
  "带初始值的变量声明，例如：float3 color = float3(1,0,0);"
  [& {:keys [type-str name init-str]}]
  (if init-str
    (T "${type-str} ${name} = ${init-str};")
    (T "${type-str} ${name};")))

(defn uniform-var-decl
  "uniform 变量声明（无初始化）：uniform float4x4 worldViewProj;"
  [& {:keys [type-str name]}]
  (T "uniform ${type-str} ${name};"))

(defn static-var-decl-init
  "static 变量声明（必须初始化）：static float4 accumColor = float4(0,0,0,0);"
  [& {:keys [type-str name init-str]}]
  (T "static ${type-str} ${name} = ${init-str};"))

;; ── 变量引用 ────────────────────────────
(defn var-ref
  "变量引用，直接返回名称。"
  [& {:keys [name]}]
  name)

;; ── 赋值 ────────────────────────────────
(defn assign
  "赋值语句：target = value;"
  [& {:keys [target-str value-str]}]
  (T "${target-str} = ${value-str};"))

;; ── 函数调用 ─────────────────────────────
(defn call
  "通用调用模板：fnName(arg1, arg2)。"
  [& {:keys [fn-name args-str]}]
  (T "${fn-name}(${args-str})"))

(defn fn-call
  "更便捷的函数调用，args 为字符串列表。"
  [& {:keys [fn-name args]}]
(call :fn-name fn-name :args-str (str/join ", " args)))

;; ── 类型转换 ─────────────────────────────
(defn type-cast
  "显式类型转换：(float)expr"
  [& {:keys [type-str expr-str]}]
  (T "(${type-str})${expr-str}"))

;; ── 成员访问 ─────────────────────────────
(defn member-access
  "对象成员访问：obj.member"
  [& {:keys [target-str member]}]
  (let [member-str (if (keyword? member) (name member) member)]
    (T "${target-str}.${member-str}")))

;; ── 控制流 ──────────────────────────────
(defn if-stmt
  "if 语句：if (cond) { body }，无 else。"
  [& {:keys [condition body]}]
  (T "if (${condition}) { ${body} }"))

(defn if-else-stmt
  "if-else 语句：if (cond) { then } else { else }"
  [& {:keys [condition then-body else-body]}]
  (T "if (${condition}) { ${then-body} } else { ${else-body} }"))

(defn while-stmt
  "while 循环：while (cond) { body }"
  [& {:keys [condition body]}]
  (T "while (${condition}) { ${body} }"))

(defn for-stmt
  "for 循环：for(init; cond; iter) { body }"
  [& {:keys [init-str cond-str iter-str body]}]
  (T "for(${init-str}; ${cond-str}; ${iter-str}) { ${body} }"))

(defn func-signature
  "函数签名：float4 main(VSInput input)"
  [& {:keys [ fn-name return-type params-str]}]
  (T "${return-type} ${fn-name}(${params-str})"))

(defn func-signature-semantic
  "带语义的函数签名：float4 main(VSInput input) : SV_POSITION"
  [& {:keys [return-type name params-str semantic]}]
  (T "${return-type} ${name}(${params-str}) : ${semantic}"))

(defn include
  [path-sym]
  (let [_path (name path-sym)]
    (T "#include \"${_path}.hlsl\"")))

(defn func-body
  "函数体（带大括号）：{ body }"
  [& {:keys [body-str]}]
  (T "{ ${body-str} }"))

(defn return-stmt
  "return 语句：return expr;"
  [& {:keys [expr-str]}]
  (T "return ${expr-str};"))

;; ── 结构体 ───────────────────────────────
(defn struct-decl
  "结构体声明：struct Name { members };"
  [& {:keys [name members-str]}]
  (T "struct ${name} { ${members-str} };"))

(defn struct-member
  "结构体成员：float3 position : SV_POSITION;"
  [& {:keys [type-str name semantic]}]
  (if semantic
    (T "${type-str} ${name} : ${semantic};")
    (T "${type-str} ${name};")))

;; ── 资源声明 ─────────────────────────────
(defn texture2d-decl
  "纹理声明：Texture2D name : register(t0);"
  [& {:keys [name register-slot]}]
  (T "Texture2D ${name} : register(${register-slot});"))

(defn sampler-decl
  "采样器声明：SamplerState name : register(s0);"
  [& {:keys [name register-slot]}]
  (T "SamplerState ${name} : register(${register-slot});"))

(defn cbuffer-decl
  "cbuffer 声明：cbuffer Name : register(b0) { members };"
  [& {:keys [name register-slot members-str]}]
  (T "cbuffer ${name} : register(${register-slot}) { ${members-str} };"))
