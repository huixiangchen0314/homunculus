(ns top.kzre.homunculus.backend.hlsl.integration-test
  "全管线集成测试：Clojure 形式 → IR1 → IR2 → Passes → HLSL 代码。"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [top.kzre.homunculus.backend.unity.backend :as unity]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.backend.shader.methods]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.recur-elim.methods]
            [top.kzre.homunculus.backend.shader.dsl :as dsl]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.elaborate.methods]
            [top.kzre.homunculus.core.types.elaborate.protocol :as elab-cfg]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (top.kzre.homunculus.backend.unity.backend UnityBackend)))

(def elab-config
  (reify elab-cfg/IElaborateConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (allow-return-closure? [_] false)
    (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (should-inline? [_ _ _] true)))

(def hlsl-frontend (hlsl-front/->HLSLFrontend))
(def hlsl-backend-inst (hlsl-backend/->HLSLBackend))
(def mock-backend (->MockBackend))

(def full-builtins (merge {} hlsl-front/builtins))

(defn compile-and-emit
  "编译 Clojure 形式并生成着色器代码。entries 为入口列表，每项 {:stage :vertex/:fragment, :fn-name \"...\"}"
  [form entries]
  (let [expanded   (walk/macroexpand-all form)
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        mutable    (mut/analyze elaborated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/run checked-fn :frontend hlsl-frontend)
        typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
        checked    (check/check-program typed {:backend mock-backend})]
    (emit/generate checked hlsl-backend-inst entries)))

(defn hlsl-contains? [hlsl substr]
  (str/includes? hlsl substr))

(deftest test-simple-literal
  (testing "数字字面量"
    (let [hlsl (compile-and-emit 42.0 [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "return 42.0;"))
      (is (hlsl-contains? hlsl "float4 main() : SV_TARGET { return frag(); }")))))

(deftest test-simple-call
  (testing "(+ 1.0 2.0)"
    (let [hlsl (compile-and-emit '(+ 1.0 2.0) [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "1.0 + 2.0"))
      (is (hlsl-contains? hlsl "return"))
      (is (hlsl-contains? hlsl "frag()")))))

(deftest test-let-binding
  (testing "let 绑定"
    (let [hlsl (compile-and-emit '(let* [x 42.0] x) [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "const float x = 42.0"))
      (is (hlsl-contains? hlsl "return x;"))
      (is (hlsl-contains? hlsl "frag()")))))

(deftest test-float4-constructor
  (testing "向量构造 float4"
    (let [hlsl (compile-and-emit '(float4 1.0 2.0 3.0 4.0) [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "float4(1.0, 2.0, 3.0, 4.0)"))
      (is (hlsl-contains? hlsl "return"))
      (is (hlsl-contains? hlsl "frag()")))))

(deftest test-if-statement
  (testing "if 表达式"
    (let [hlsl (compile-and-emit '(if true 1.0 0.0) [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "if (true)"))
      (is (hlsl-contains? hlsl "return 1.0"))
      (is (hlsl-contains? hlsl "else"))
      (is (hlsl-contains? hlsl "frag()")))))

(deftest test-while-loop
  (testing "while loop with mutable variable"
    (let [hlsl (compile-and-emit '(loop* [i 0.0] (if (< i 10.0) (recur (+ i 1.0)) i)) [{:stage :fragment :fn-name "frag"}])]
      (is (hlsl-contains? hlsl "while"))
      (is (hlsl-contains? hlsl "i = "))
      (is (hlsl-contains? hlsl "frag()")))))

(deftest test-function-definition
  (testing "顶层函数定义"
    (let [hlsl (compile-and-emit '(def square (fn* [^:float x] (* x x))) [{:stage :fragment :fn-name "square"}])]
      (is (hlsl-contains? hlsl "float square(float x)"))
      (is (hlsl-contains? hlsl "return x * x"))
      (is (hlsl-contains? hlsl "float4 main() : SV_TARGET { return square(); }")))))

(deftest test-vertex-shader-entry
  (testing "顶点着色器入口点生成"
    (let [hlsl (compile-and-emit '(top.kzre.homunculus.backend.shader.dsl/defshader
                                    :vertex vs-main
                                    [^:SV_Position ^:float4 pos]
                                    pos)
                                 [{:stage :vertex :fn-name "vs-main"}])]
      (is (str/includes? hlsl "float4 vs_main(float4 pos : SV_Position)"))
      (is (str/includes? hlsl "struct VSInput"))
      (is (str/includes? hlsl "float4 pos : SV_Position;"))
      (is (str/includes? hlsl "struct VSOutput"))
      (is (str/includes? hlsl "float4 pos : SV_POSITION;"))
      (is (str/includes? hlsl "output.pos = vs_main(input.pos);"))
      (is (str/includes? hlsl "VSOutput main(VSInput input)")))))

(deftest test-vertex-shader-with-input
  (testing "顶点着色器入口点生成（包含额外输入）"
    (let [hlsl (compile-and-emit '(top.kzre.homunculus.backend.shader.dsl/defshader
                                    :vertex vs-main
                                    [^:SV_Position ^:float4 pos
                                     ^:TEXCOORD0  ^:float2 uv]
                                    pos)
                                 [{:stage :vertex :fn-name "vs-main"}])]
      (is (str/includes? hlsl "float4 vs_main(float4 pos : SV_Position, float2 uv : TEXCOORD0)"))
      (is (str/includes? hlsl "struct VSInput"))
      (is (str/includes? hlsl "float2 uv : TEXCOORD0;"))
      (is (str/includes? hlsl "output.pos = vs_main(input.pos, input.uv);")))))

(deftest test-unity-backend-fragment
  (testing "Unity 后端生成 ShaderLab 片段着色器"
    (let [hlsl-frontend (hlsl-front/->HLSLFrontend)
          unity-backend (unity/->UnityBackend)
          mock-backend  (->MockBackend)
          full-builtins (merge {} hlsl-front/builtins)
          compile (fn [form entries]
                    (let [expanded   (walk/macroexpand-all form)
                          ir1-root   (ir1/->ir1 expanded)
                          ir2-roots  (ir2/lower [ir1-root])
                          no-recur   (mapv recur-elim/eliminate ir2-roots)
                          elaborated (elaborate/elaborate no-recur elab-config)
                          mutable    (mut/analyze elaborated)
                          checked-fn (builtin/check mutable full-builtins)
                          inferred   (infer/run checked-fn :frontend hlsl-frontend)
                          typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
                          checked    (check/check-program typed {:backend mock-backend})]
                      (emit/generate checked unity-backend entries)))
          hlsl (compile 42.0 [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "Shader \"Custom/frag\""))
      (is (str/includes? hlsl "SubShader"))
      (is (str/includes? hlsl "HLSLPROGRAM"))
      (is (str/includes? hlsl "#pragma fragment frag"))
      (is (str/includes? hlsl "return 42.0;"))
      (is (str/includes? hlsl "ENDHLSL")))))


(deftest test-vector-abs
  (testing "向量 abs"
    (let [hlsl (compile-and-emit '(abs (float2 -1.0 2.0)) [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "abs(float2(-1.0, 2.0))"))
      (is (str/includes? hlsl "return")))))

(deftest test-vector-max
  (testing "向量 max"
    (let [hlsl (compile-and-emit '(max (float3 1.0 2.0 3.0) (float3 3.0 2.0 1.0)) [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "max(float3(1.0, 2.0, 3.0), float3(3.0, 2.0, 1.0))"))
      (is (str/includes? hlsl "return")))))

(deftest test-swizzle-xy
  (let [hlsl (compile-and-emit '(sw-xy (float4 1.0 2.0 3.0 4.0)) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "float4(1.0, 2.0, 3.0, 4.0).xy"))
    (is (str/includes? hlsl "return"))))


(deftest test-global-uniform
  (testing "全局 uniform 常量"
    (let [hlsl (compile-and-emit '(def ^:uniform myColor (float4 1.0 0.0 0.0 1.0))
                                 [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "uniform float4 myColor"))
      (is (str/includes? hlsl "float4(1.0, 0.0, 0.0, 1.0)")))))

(deftest test-cbuffer-basic
  (testing "cbuffer 声明基本形式"
    (let [hlsl (compile-and-emit
                 '(top.kzre.homunculus.backend.shader.dsl/defcbuffer myCB :register 0
                                                                     [worldMatrix (float4x4 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0)]
                                                                     [lightDir    (float3 0.0 0.0 1.0)])
                 [{:stage :vertex :fn-name "vs-main"}])]
      (is (str/includes? hlsl "cbuffer myCB : register(b0) {"))
      (is (str/includes? hlsl "float4x4 worldMatrix = float4x4(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);"))
      (is (str/includes? hlsl "float3 lightDir = float3(0.0, 0.0, 1.0);"))
      (is (str/includes? hlsl "};")))))
