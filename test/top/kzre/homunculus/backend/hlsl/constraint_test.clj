(ns top.kzre.homunculus.backend.hlsl.constraint-test
  "HLSL 后端集成测试：使用新的约束系统替换 typed pass。
   暂时只测试基本表达式，重载、用户定义函数等尚在开发中。"
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.integration-test :refer [elab-config
                                                                       full-builtins
                                                                       hlsl-frontend]]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]))

(defn compile-and-emit-cs
  "使用约束求解系统替换 typed pass。"
  [form entries]
  (let [expanded   (walk/macroexpand-all form)
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        mutable    (mut/analyze elaborated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/run checked-fn :frontend hlsl-frontend)
        typed      (cs/process inferred
                               {:frontend hlsl-frontend
                                :env (merge {} hlsl-front/builtins)})
        checked    (check/check-program typed {:backend (hlsl-backend/->HLSLBackend)})]
    (emit/generate checked (hlsl-backend/->HLSLBackend) entries)))

(deftest test-simple-literal-cs
  (let [hlsl (compile-and-emit-cs 42.0 [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "return 42.0;"))
    (is (str/includes? hlsl "float4 main() : SV_TARGET { return frag(); }"))))

(deftest test-builtin-call-cs
  (let [hlsl (compile-and-emit-cs '(+ 1.0 2.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "1.0 + 2.0"))
    (is (str/includes? hlsl "return"))))

(deftest test-let-binding-cs
  (let [hlsl (compile-and-emit-cs '(let* [x 42.0] x) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "const float x = 42.0"))
    (is (str/includes? hlsl "return x;"))))

(deftest test-if-statement-cs
  (let [hlsl (compile-and-emit-cs '(if true 1.0 0.0) [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "if (true)"))
    (is (str/includes? hlsl "return 1.0"))
    (is (str/includes? hlsl "else"))))

(deftest test-abs-float2-cs
  (testing "泛型函数 abs 在向量上的实例化"
    (let [hlsl (compile-and-emit-cs '(abs (float2 -1.0 2.0))
                                    [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "abs(float2(-1.0, 2.0))"))
      (is (str/includes? hlsl "return")))))

;; ── 以下测试因约束系统尚未支持重载及用户定义函数，暂时跳过 ──
#_(deftest test-overload-float3-cs
    (let [hlsl (compile-and-emit-cs '(float3 (float2 1.0 2.0) 3.0)
                                    [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "float3(float2(1.0, 2.0), 3.0)"))
      (is (str/includes? hlsl "return"))))

#_(deftest test-function-definition-cs
    (let [hlsl (compile-and-emit-cs
                 '(do
                    (def square (fn* [^:float x] (* x x)))
                    (defshader :fragment fs []
                               (square 3.0)))
                 [{:stage :fragment :fn-name "fs"}])]
      (is (str/includes? hlsl "float square(float x)"))
      (is (str/includes? hlsl "return (x * x)"))
      (is (str/includes? hlsl "float4 main() : SV_TARGET { return fs(); }"))))

#_(deftest test-abs-float2-cs
    (let [hlsl (compile-and-emit-cs '(abs (float2 -1.0 2.0))
                                    [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "abs(float2(-1.0, 2.0))"))
      (is (str/includes? hlsl "return"))))