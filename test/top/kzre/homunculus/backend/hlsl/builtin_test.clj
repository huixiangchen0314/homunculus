(ns top.kzre.homunculus.backend.hlsl.builtin-test
  "测试新增 HLSL 内置函数的类型推导和代码生成。"
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [top.kzre.homunculus.backend.hlsl.integration-test :refer [compile-and-emit]]))

;; ── 类型转换 ──
(deftest test-half-conversion
  (let [hlsl (compile-and-emit '(half 1.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "half(1.0)"))
    (is (str/includes? hlsl "return"))))

(deftest test-double-conversion
  (let [hlsl (compile-and-emit '(double 1.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "double(1.0)"))
    (is (str/includes? hlsl "return"))))

(deftest test-uint-conversion
  (let [hlsl (compile-and-emit '(uint 1) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "uint(1)"))
    (is (str/includes? hlsl "return"))))

(deftest test-uint2-constructor
  (let [hlsl (compile-and-emit '(uint2 1.0 2.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "uint2(1.0, 2.0)"))
    (is (str/includes? hlsl "return"))))

;; ── 新增数学函数 ──
(deftest test-degrees
  (let [hlsl (compile-and-emit '(degrees 3.14159) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "degrees(3.14159)"))
    (is (str/includes? hlsl "return"))))

(deftest test-radians
  (let [hlsl (compile-and-emit '(radians 180.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "radians(180.0)"))
    (is (str/includes? hlsl "return"))))

(deftest test-sign
  (let [hlsl (compile-and-emit '(sign -5.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "sign(-5.0)"))
    (is (str/includes? hlsl "return"))))

(deftest test-faceforward
  (let [hlsl (compile-and-emit '(faceforward (float3 0.0 0.0 1.0) (float3 0.0 0.0 -1.0) (float3 0.0 0.0 1.0))
                               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "faceforward(float3(0.0, 0.0, 1.0), float3(0.0, 0.0, -1.0), float3(0.0, 0.0, 1.0))"))
    (is (str/includes? hlsl "return"))))

;; ── 泛型长度/距离 ──
(deftest test-length-float2
  (let [hlsl (compile-and-emit '(length (float2 3.0 4.0)) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "length(float2(3.0, 4.0))"))
    (is (str/includes? hlsl "return"))))

(deftest test-length-float4
  (let [hlsl (compile-and-emit '(length (float4 1.0 2.0 3.0 4.0)) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "length(float4(1.0, 2.0, 3.0, 4.0))"))
    (is (str/includes? hlsl "return"))))

(deftest test-distance-float2
  (let [hlsl (compile-and-emit '(distance (float2 0.0 0.0) (float2 3.0 4.0)) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "distance(float2(0.0, 0.0), float2(3.0, 4.0))"))
    (is (str/includes? hlsl "return"))))

;; ── 纹理采样变体 ──
(deftest test-sample-level
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (sample-level tex samp (float2 0.5 0.5) 0.0))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.SampleLevel(samp, float2(0.5, 0.5), 0.0)"))
    (is (str/includes? hlsl "return"))))

(deftest test-sample-bias
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (sample-bias tex samp (float2 0.5 0.5) 0.1))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.SampleBias(samp, float2(0.5, 0.5), 0.1)"))
    (is (str/includes? hlsl "return"))))

(deftest test-sample-grad
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (sample-grad tex samp (float2 0.5 0.5) (float2 1.0 0.0) (float2 0.0 1.0)))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.SampleGrad(samp, float2(0.5, 0.5), float2(1.0, 0.0), float2(0.0, 1.0))"))
    (is (str/includes? hlsl "return"))))

(deftest test-sample-cmp
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (sample-cmp tex samp (float2 0.5 0.5) 0.5))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.SampleCmp(samp, float2(0.5, 0.5), 0.5)"))
    (is (str/includes? hlsl "return"))))

(deftest test-sample-cmp-level-zero
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (sample-cmp-level-zero tex samp (float2 0.5 0.5) 0.5))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.SampleCmpLevelZero(samp, float2(0.5, 0.5), 0.5)"))
    (is (str/includes? hlsl "return"))))

(deftest test-gather
  (let [hlsl (compile-and-emit
               '(do (def tex (texture2D 0))
                    (def samp (sampler-state 1))
                    (gather tex samp (float2 0.5 0.5)))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "tex.Gather(samp, float2(0.5, 0.5))"))
    (is (str/includes? hlsl "return"))))