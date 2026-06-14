(ns top.kzre.homunculus.backend.hlsl.struct-test
  "HLSL 结构体生成的单元测试。"
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(def backend (hlsl/->HLSLBackend))

(deftest test-empty-params
  (testing "空参数列表应返回空字符串"
    (is (= "" (sp/shader-struct-from-params backend "Empty" [])))))

(deftest test-single-param-without-semantic
  (testing "单个参数，无语义"
    (let [params [{:name "x" :type "float" :semantic nil}]
          result (sp/shader-struct-from-params backend "MyStruct" params)]
      (is (str/includes? result "struct MyStruct {"))
      (is (str/includes? result "    float x;"))
      (is (not (str/includes? result " : "))))))  ; 无语义不应有冒号

(deftest test-single-param-with-semantic
  (testing "单个参数，带语义"
    (let [params [{:name "pos" :type "float4" :semantic "SV_POSITION"}]
          result (sp/shader-struct-from-params backend "VSOutput" params)]
      (is (str/includes? result "struct VSOutput {"))
      (is (str/includes? result "    float4 pos : SV_POSITION;")))))

(deftest test-multiple-params-mixed
  (testing "多个参数，混合语义"
    (let [params [{:name "pos" :type "float4" :semantic "SV_POSITION"}
                  {:name "uv"  :type "float2" :semantic "TEXCOORD0"}
                  {:name "color" :type "float4" :semantic nil}]
          result (sp/shader-struct-from-params backend "VertexOutput" params)]
      (is (str/includes? result "struct VertexOutput {"))
      (is (str/includes? result "    float4 pos : SV_POSITION;"))
      (is (str/includes? result "    float2 uv : TEXCOORD0;"))
      (is (str/includes? result "    float4 color;"))   ; 无语义无冒号
      (is (not (str/includes? result "color :"))))))


(deftest test-entry-wrapper-vertex
  (let [backend (hlsl/->HLSLBackend)
        result (sp/shader-entry-wrapper backend :vertex "vs_main"
                                        [{:name "pos" :type "float4" :semantic "SV_Position"}]
                                        [{:name "pos" :type "float4" :semantic "SV_POSITION"}])]
    (is (str/includes? result "struct VSInput"))
    (is (str/includes? result "float4 pos : SV_Position;"))
    (is (str/includes? result "struct VSOutput"))
    (is (str/includes? result "float4 pos : SV_POSITION;"))
    (is (str/includes? result "output.pos = vs_main(input.pos);"))
    (is (str/includes? result "VSOutput main(VSInput input)"))))