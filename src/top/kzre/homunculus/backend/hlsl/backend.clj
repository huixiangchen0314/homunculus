(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 对 IShaderBackend 的实现。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.utils :as utils]
    [top.kzre.homunculus.backend.shader.protocol :as sp]
    [top.kzre.homunculus.backend.util.format :as fmt]
    [top.kzre.homunculus.backend.util.naming :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defrecord HLSLBackend []
  sp/IShaderBackend

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
      :else (throw (ex-info (str "HLSL unsupported literal: " val) {:val val}))))

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

  (shader-if [_ test then else]
    (if else
      (str "if (" test ") { " then " } else { " else " }")
      (str "if (" test ") { " then " }")))

  (shader-while [_ test body]
    (str "while (" test ") { " body " }"))

  (shader-block [_ stmts]
    (str "{\n" (fmt/indent 1) (str/join (str ";\n" (fmt/indent 1)) stmts) ";\n}"))

  (shader-function-decl [this name params return-type body]
    (str return-type " " (n/safe-name name) "(" (fmt/comma-sep params) ")\n"
         body))   ;; body 已经格式化好（含大括号或单语句）

  (shader-return [_ expr]
    (str "return " expr ";"))

  (shader-call [this fn-name args]
    (let [sym (symbol fn-name)
          info (get utils/builtin-map sym)]
      (if info
        (cond
          (:infix info)
          (if (= 2 (count args))
            (str "(" (first args) (:op info) (second args) ")")
            (throw (ex-info "Infix op requires 2 args" {:fn fn-name :args args})))

          (:sample info)   ;; 采样统一处理
          (let [[tex sam & rest] args]
            (str tex ".Sample(" sam (when (seq rest) (str ", " (str/join ", " rest))) ")"))

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

  ;; 核心改变：shader-program 接管入口生成
  ;; 在 backend/hlsl/backend.clj 中替换原有的 shader-program 实现
  (shader-program [this functions structs globals stage entry-fn-name]
    (let [safe-entry-name (n/safe-name entry-fn-name)   ;; 关键修复
          body (str/join "\n\n" (filter seq
                                        [(str/join "\n" globals)
                                         (str/join "\n" structs)
                                         (str/join "\n" functions)]))
          fn-str (some (fn [f]
                         (when (.startsWith f (str "float4 " safe-entry-name "("))
                           f))
                       functions)
          entry (case stage
                  :vertex
                  (if fn-str
                    (let [params-part (second (re-find #"\(([^)]*)\)" fn-str))
                          ;; backend/hlsl/backend.clj 的 shader-program 内，替换 params 解析部分
                          params (when params-part
                                   (for [p (str/split params-part #",")]
                                     (let [p (str/trim p)
                                           ;; 先分割语义部分： "float4 pos : SV_Position" -> ["float4 pos" "SV_Position"]
                                           [type+name semantic] (str/split p #" : ")
                                           ;; 再分割类型和名字： "float4 pos" -> ["float4" "pos"]
                                           [type name]          (str/split (str/trim type+name) #" ")]
                                       {:name (or name "unknown")
                                        :type type
                                        :semantic semantic})))
                          input-members (mapv #(select-keys % [:name :type :semantic]) params)
                          input-struct  (sp/shader-struct-decl this "VSInput" input-members)
                          output-struct (sp/shader-struct-decl this "VSOutput" [{:name "pos" :type "float4" :semantic "SV_POSITION"}])
                          call-args     (str/join ", " (map #(str "input." (:name %)) params))
                          entry-body    (str "VSOutput output;\n"
                                             "    output.pos = " safe-entry-name "(" call-args ");\n"
                                             "    return output;")]
                      (str input-struct "\n" output-struct "\n"
                           "VSOutput main(VSInput input) {\n"
                           (fmt/indent 1) entry-body "\n"
                           "}"))
                    (throw (ex-info "Vertex shader requires a defshader entry with typed parameters"
                                    {:stage stage :entry-fn-name entry-fn-name})))
                  :fragment
                  (str "float4 main() : SV_TARGET { return " safe-entry-name "(); }")
                  (str "void main() { " safe-entry-name "(); }"))]
      (str body "\n\n" entry)))

  (shader-resource-decl [_ name res-type args]
    (let [reg-arg (first args)
          reg-val (when (and reg-arg (= (ir2p/kind reg-arg) :literal)) (:val reg-arg))]
      (case res-type
        :texture2D (str "Texture2D<float4> " name " : register(t" reg-val ");")
        :sampler   (str "SamplerState " name " : register(s" reg-val ");")
        :cbuffer   (str "cbuffer " name " : register(b" reg-val ") { ... };")
        (throw (ex-info "Unknown resource type" {:type res-type}))))))