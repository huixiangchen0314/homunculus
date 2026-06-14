(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 对 IShaderBackend 的实现。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.utils :as utils]
    [top.kzre.homunculus.backend.shader.protocol :as sp]
    [top.kzre.homunculus.backend.util.format :as fmt]
    [top.kzre.homunculus.backend.shader.emit :as e]
    [top.kzre.homunculus.backend.util.naming :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.model :as t]))

;; ── HLSL 类型映射 ──
(def type-map
  {:int     "int"
   :float   "float"
   :int64   "int"
   :int32   "int"
   :int16   "int16_t"
   :uint64  "uint"
   :uint32  "uint"
   :float64 "double"
   :float32 "float"
   :float16 "half"
   :bool    "bool"
   :nil     "void"
   :vector  "float4"
   :float2  "float2"
   :float3  "float3"
   :float4  "float4"
   :float3x3 "float3x3"
   :float4x4 "float4x4"
   :string  "float"})

;; ── HLSL 内置函数/运算符映射 ──
(def builtin-map
  {'+         {:op " + " :infix true}
   '-         {:op " - " :infix true}
   '*         {:op " * " :infix true}
   '/         {:op " / " :infix true}

   '<         {:op " < " :infix true}
   '>         {:op " > " :infix true}
   '<=        {:op " <= " :infix true}
   '>=        {:op " >= " :infix true}
   '==        {:op " == " :infix true}
   '!=        {:op " != " :infix true}
   '!         {:fn "!" :prefix true}
   '&&        {:op " && " :infix true}
   '||        {:op " || " :infix true}

   'pow       {:fn "pow"}
   'not       {:fn "!" :prefix true}
   'inc       {:fn "inc"}
   'dec       {:fn "dec"}
   'abs       {:fn "abs"}
   'min       {:fn "min"}
   'max       {:fn "max"}
   'clamp     {:fn "clamp"}
   'lerp      {:fn "lerp"}
   'sqrt      {:fn "sqrt"}
   'saturate  {:fn "saturate"}
   'sin       {:fn "sin"}
   'cos       {:fn "cos"}
   'tan       {:fn "tan"}
   'asin      {:fn "asin"}
   'acos      {:fn "acos"}
   'atan      {:fn "atan"}
   'atan2     {:fn "atan2"}
   'exp       {:fn "exp"}
   'log       {:fn "log"}
   'log2      {:fn "log2"}
   'log10     {:fn "log10"}
   'floor     {:fn "floor"}
   'ceil      {:fn "ceil"}
   'round     {:fn "round"}
   'trunc     {:fn "trunc"}
   'frac      {:fn "frac"}
   'ddx       {:fn "ddx"}
   'ddy       {:fn "ddy"}
   'fwidth    {:fn "fwidth"}
   'fmod      {:fn "fmod"}
   'dot       {:fn "dot"}
   'cross     {:fn "cross"}
   'normalize {:fn "normalize"}
   'length    {:fn "length"}
   'distance  {:fn "distance"}
   'reflect   {:fn "reflect"}
   'refract   {:fn "refract"}
   'transpose {:fn "transpose"}
   'mul       {:fn "mul"}
   'tex2D     {:fn "tex2D" :sample true}
   'texCube   {:fn "texCUBE" :sample true}

   'float     {:fn "float"}
   'int       {:fn "int"}
   'bool      {:fn "bool"}

   'int2      {:fn "int2"}
   'int3      {:fn "int3"}
   'int4      {:fn "int4"}
   'bool2     {:fn "bool2"}
   'bool3     {:fn "bool3"}
   'bool4     {:fn "bool4"}

   'float4x4 {:fn "float4x4"}

   'sample { :sample true}
   'sample-level {:sample "SampleLevel"}
   'sample-bias  {:sample "SampleBias"}
   'sample-grad  {:sample "SampleGrad"}
   'sample-cmp   {:sample "SampleCmp"}

   ;; Swizzle 支持
   'sw-x    {:swizzle "x"}
   'sw-y    {:swizzle "y"}
   'sw-z    {:swizzle "z"}
   'sw-w    {:swizzle "w"}
   'sw-xy   {:swizzle "xy"}
   'sw-xz   {:swizzle "xz"}
   'sw-yz   {:swizzle "yz"}
   'sw-zw   {:swizzle "zw"}
   'sw-xyz  {:swizzle "xyz"}
   'sw-rgb  {:swizzle "rgb"}
   'sw-xyzw {:swizzle "xyzw"}
   'sw-rgba {:swizzle "rgba"}})

(defrecord HLSLBackend []
  sp/IShaderBackend

  (shader-type [_ ir-type]
    (let [name (:name ir-type)]
      (or (get type-map name) "float")))

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
    (if (get builtin-map (symbol name))
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
         body))

  (shader-return [_ expr]
    (str "return " expr ";"))

  (shader-call [this fn-name args]
    (let [sym (symbol fn-name)
          info (get builtin-map sym)]
      (if info
        (cond
          (:infix info)
          (if (= 2 (count args))
            (str (first args) (:op info) (second args))
            (throw (ex-info "Infix op requires 2 args" {:fn fn-name :args args})))

          (:swizzle info)
          (str (first args) "." (:swizzle info))

          (:sample info)
          (let [method (if (string? (:sample info)) (:sample info) "Sample")
                [tex sam & rest] args]
            (str tex "." method "(" sam (when (seq rest) (str ", " (str/join ", " rest))) ")"))

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

  (sp/shader-program [this functions structs globals entry-specs]
    (let [all-parts (concat globals structs functions)
          body (when (seq all-parts) (str/join "\n\n" all-parts))
          entries (for [{:keys [stage fn-name input-params output-params]} entry-specs]
                    (sp/shader-entry-wrapper this stage fn-name input-params output-params))]
      (str body "\n\n" (str/join "\n\n" entries))))

  (shader-resource-decl [this res-name res-type args]
    (let [reg-arg (first args)
          reg-val (or (when (and reg-arg (= (ir2p/kind reg-arg) :literal)) (:val reg-arg)) 0)]
      (case res-type
        :texture2D (str "Texture2D<float4> " res-name " : register(t" reg-val ");")
        :sampler   (str "SamplerState " res-name " : register(s" reg-val ");")
        :cbuffer
        (let [members-node (second args)
              member-kvs   (:kvs members-node)
              member-pairs (partition 2 member-kvs)
              member-strs  (for [[key-node val-node] member-pairs
                                 :let [member-name (name (:val key-node))  ;; 此处 name 是 clojure.core/name，因为外层参数已改名
                                       member-val  (e/emit val-node this)
                                       member-ty   (or (get-in val-node [:attrs :type])
                                                       (t/->TCon :float4))
                                       type-str    (sp/shader-type this member-ty)]]
                             (str "    " type-str " " member-name
                                  (when member-val (str " = " member-val)) ";"))]
          (str "cbuffer " res-name " : register(b" reg-val ") {\n"
               (str/join "\n" member-strs) "\n"
               "};"))
        (throw (ex-info "Unknown resource type" {:type res-type})))))

  (shader-struct-from-params [_ struct-name params]
    (if (seq params)
      (let [member-strs (map (fn [p]
                               (str "    " (:type p) " " (:name p)
                                    (when (:semantic p) (str " : " (:semantic p))) ";"))
                             params)]
        (str "struct " struct-name " {\n"
             (str/join "\n" member-strs) "\n"
             "};"))
      ""))

  (shader-entry-wrapper [this stage entry-fn-name input-params output-params]
    (let [safe-name (n/safe-name entry-fn-name)]
      (case stage
        :vertex
        (let [input-struct  (sp/shader-struct-from-params this "VSInput" input-params)
              output-struct (sp/shader-struct-from-params this "VSOutput" output-params)
              call-args     (str/join ", " (map #(str "input." (:name %)) input-params))
              output-pos    (if (seq output-params)
                              (:name (first output-params))
                              "pos")
              entry-body    (str "VSOutput output;\n"
                                 "    output." output-pos " = " safe-name "(" call-args ");\n"
                                 "    return output;")]
          (str (when (seq input-params) (str input-struct "\n"))
               (when (seq output-params) (str output-struct "\n"))
               "VSOutput main(VSInput input) {\n"
               (fmt/indent 1) entry-body "\n"
               "}"))
        :fragment
        (if (seq input-params)
          (let [param-strs (map (fn [p] (str (:type p) " " (:name p) " : " (:semantic p))) input-params)
                call-args  (str/join ", " (map :name input-params))]
            (str "float4 main(" (str/join ", " param-strs) ") : SV_TARGET {\n"
                 (fmt/indent 1) "return " safe-name "(" call-args ");\n"
                 "}"))
          (str "float4 main() : SV_TARGET { return " safe-name "(); }"))
        )))

  (shader-global-decl [this name ir-type init-expr]
    (let [type-str (sp/shader-type this ir-type)
          var-name (n/safe-name name)]
      (str "uniform " type-str " " var-name
           (when init-expr (str " = " init-expr)) ";"))))