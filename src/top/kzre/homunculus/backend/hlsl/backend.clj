(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 对 IShaderBackend 的实现。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.utils :as utils]
    [top.kzre.homunculus.backend.shader.protocol :as sp]
    [top.kzre.homunculus.backend.util.format :as fmt]
    [top.kzre.homunculus.backend.shader.emit :as e]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.backend.util.naming :as n]
    [top.kzre.homunculus.core.ir2.protocol :as ir2p]
    [top.kzre.homunculus.core.types.model :as t]))

;; ── 类型映射（新增 half/double/uint 等）──
(def type-map
  {:int     "int"
   :float   "float"
   :half    "half"
   :double  "double"
   :uint    "uint"
   :int2    "int2"
   :int3    "int3"
   :int4    "int4"
   :uint2   "uint2"
   :uint3   "uint3"
   :uint4   "uint4"
   :bool    "bool"
   :bool2   "bool2"
   :bool3   "bool3"
   :bool4   "bool4"
   :float2  "float2"
   :float3  "float3"
   :float4  "float4"
   :float3x3 "float3x3"
   :float4x4 "float4x4"
   :nil     "void"
   :string  "float"
   :texture2D "Texture2D<float4>"
   :sampler   "SamplerState"})

;; ── 内置函数/运算符映射（增加新函数）──
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
   'abs       {:fn "abs"}
   'min       {:fn "min"}
   'max       {:fn "max"}
   'clamp     {:fn "clamp"}
   'lerp      {:fn "lerp"}
   'saturate  {:fn "saturate"}
   'sin       {:fn "sin"}
   'cos       {:fn "cos"}
   'tan       {:fn "tan"}
   'asin      {:fn "asin"}
   'acos      {:fn "acos"}
   'atan      {:fn "atan"}
   'atan2     {:fn "atan2"}
   'sqrt      {:fn "sqrt"}
   'exp       {:fn "exp"}
   'log       {:fn "log"}
   'log2      {:fn "log2"}
   'log10     {:fn "log10"}
   'floor     {:fn "floor"}
   'ceil      {:fn "ceil"}
   'round     {:fn "round"}
   'trunc     {:fn "trunc"}
   'frac      {:fn "frac"}
   'fmod      {:fn "fmod"}
   'ddx       {:fn "ddx"}
   'ddy       {:fn "ddy"}
   'fwidth    {:fn "fwidth"}
   'degrees   {:fn "degrees"}
   'radians   {:fn "radians"}
   'sign      {:fn "sign"}
   'step      {:fn "step"}
   'smoothstep {:fn "smoothstep"}
   'faceforward {:fn "faceforward"}
   'dot       {:fn "dot"}
   'cross     {:fn "cross"}
   'normalize {:fn "normalize"}
   'length    {:fn "length"}
   'distance  {:fn "distance"}
   'reflect   {:fn "reflect"}
   'refract   {:fn "refract"}
   'mul       {:fn "mul"}
   'transpose {:fn "transpose"}
   'float     {:fn "float"}
   'int       {:fn "int"}
   'bool      {:fn "bool"}
   'half      {:fn "half"}
   'double    {:fn "double"}
   'uint      {:fn "uint"}
   'int2      {:fn "int2"}
   'int3      {:fn "int3"}
   'int4      {:fn "int4"}
   'uint2     {:fn "uint2"}
   'uint3     {:fn "uint3"}
   'uint4     {:fn "uint4"}
   'bool2     {:fn "bool2"}
   'bool3     {:fn "bool3"}
   'bool4     {:fn "bool4"}
   'tex2D     {:fn "tex2D" :sample true}
   'texCube   {:fn "texCUBE" :sample true}
   'tex1D     {:fn "tex1D" :sample true}
   'tex3D     {:fn "tex3D" :sample true}
   'sample        {:sample true}                      ;; 默认 Sample
   'sample-level  {:sample "SampleLevel"}
   'sample-bias   {:sample "SampleBias"}
   'sample-grad   {:sample "SampleGrad"}
   'sample-cmp    {:sample "SampleCmp"}
   'sample-cmp-level-zero {:sample "SampleCmpLevelZero"}
   'gather        {:sample "Gather"}
   ;; Swizzle
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

        :geometry
        (let [safe-name (n/safe-name entry-fn-name)
              max-count (or (some-> output-params first :maxvertexcount) 3)
              in-struct-name "GSInput"
              out-struct-name "GSOutput"
              in-struct  (sp/shader-struct-from-params this in-struct-name input-params)
              out-struct (sp/shader-struct-from-params this out-struct-name output-params)]
          (str (when (seq input-params) (str in-struct "\n"))
               (when (seq output-params) (str out-struct "\n"))
               "[maxvertexcount(" max-count ")]\n"
               "void main(triangle " in-struct-name " input[3], inout TriangleStream<" out-struct-name "> stream) {\n"
               (fmt/indent 1) safe-name "(input, stream);\n"
               "}"))
        :compute
        (let [safe-name (n/safe-name entry-fn-name)
              numthreads (or (some-> output-params first :numthreads) [1 1 1])]
          (str "[numthreads(" (str/join ", " numthreads) ")]\n"
               "void main() {\n"
               (fmt/indent 1) safe-name "();\n"
               "}"))
        )))

  (shader-global-decl [this name ir-type init-expr]
    (let [type-str (sp/shader-type this ir-type)
          var-name (n/safe-name name)]
      (str "uniform " type-str " " var-name
           (when init-expr (str " = " init-expr)) ";")))


  tp/IBackendInfo
  (prims [_] [])
  (builtin-type? [_ ty-name] (contains? #{:int :float :bool :float2 :float3 :float4 :float4x4} ty-name))
  (strictness [_] {:strict false})
  (type-conversion [_ src-ty dst-ty]
    ;; 允许 int 和 float 之间相互转换，代价较低
    (cond
      (and (= (:name src-ty) :int) (= (:name dst-ty) :float)) 1
      (and (= (:name src-ty) :float) (= (:name dst-ty) :int)) 2
      :else nil))
  (resolve-container [_ container-ty] container-ty)
  (backend-container-type [_ kind element-ty shape] (t/->TCon :float4))


  )