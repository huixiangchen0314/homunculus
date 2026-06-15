(ns top.kzre.homunculus.backend.hlsl.constraint-test
  "HLSL 后端集成测试：使用新的约束系统替换 typed pass。
   目前支持基本表达式、函数定义、let、if、向量、泛型实例化。
   while 循环和复杂 let 仍待完善。"
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
  (let [hlsl (compile-and-emit-cs '(abs (float2 -1.0 2.0))
                                  [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "abs(float2(-1.0, 2.0))"))
    (is (str/includes? hlsl "return"))))

;; ── 8. 函数定义与调用 ──
(deftest test-function-def-and-call-cs
  (let [hlsl (compile-and-emit-cs
               '(do
                  (def double (fn* [^:float x] (+ x x)))
                  (double 3.0))
               [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "float double(float x)"))
    (is (str/includes? hlsl "return x + x;"))))

;; ── 9. while 循环（暂时跳过）──
#_(deftest test-while-loop-cs
    (let [hlsl (compile-and-emit-cs
                 '(loop* [i 0.0] (if (< i 10.0) (recur (+ i 1.0)) i))
                 [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "while"))
      (is (str/includes? hlsl "i = "))
      (is (str/includes? hlsl "return i;"))))

;; ── 10. vector 构造 ──
(deftest test-vector-float4-cs
  (let [hlsl (compile-and-emit-cs '(float4 1.0 2.0 3.0 4.0)
                                  [{:stage :fragment :fn-name "frag"}])]
    (is (str/includes? hlsl "float4(1.0, 2.0, 3.0, 4.0)"))
    (is (str/includes? hlsl "return"))))

;; ── 11. 复杂 let（暂时跳过）──
#_(deftest test-let-generic-builtin-cs
    (let [hlsl (compile-and-emit-cs
                 '(let* [x (float2 1.0 2.0)
                         y (abs x)]
                    (+ (sw-x y) 3.0))
                 [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "float2 x = float2(1.0, 2.0)"))
      (is (str/includes? hlsl "abs(x)"))
      (is (str/includes? hlsl "y.x + 3.0"))
      (is (str/includes? hlsl "return"))))