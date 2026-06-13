(ns top.kzre.homunculus.backend.hlsl.integration-test
  "全管线集成测试：Clojure 形式 → IR1 → IR2 → Passes → HLSL 代码。"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.recur-elim.methods]
            [top.kzre.homunculus.backend.shader.dsl :as dsl]
            [top.kzre.homunculus.core.types.annotate-meta :as am]
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
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

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

(defn compile-and-emit [form entry-stage entry-fn-name]
  (let [expanded   (macroexpand form)   ;; 展开宏，确保 defshader 等被处理
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        known-types (set (p/frontend-types hlsl-frontend))
        annotated  (am/annotate elaborated known-types)
        mutable    (mut/analyze annotated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/run checked-fn :frontend hlsl-frontend)
        typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
        checked    (check/check-program typed {:backend mock-backend})
        roots      (if (some #(= (ir2p/kind %) :define) checked)
                     checked
                     (let [body (if (= 1 (count checked))
                                  (first checked)
                                  (m/->BlockNode checked nil nil nil))
                           ret-ty (get-in body [:attrs :type])]
                       [(m/->DefineNode (symbol entry-fn-name)
                                        (m/->LambdaNode [] body [] nil nil nil nil)
                                        nil nil nil nil)]))]
    (emit/generate roots hlsl-backend-inst entry-stage entry-fn-name)))

(defn hlsl-contains? [hlsl substr]
  (str/includes? hlsl substr))

(deftest test-simple-literal
  (testing "数字字面量"
    (let [hlsl (compile-and-emit 42.0 :fragment "main")]
      (is (hlsl-contains? hlsl "return 42.0;"))
      ;; 不再检查 void main()
      )))

(deftest test-simple-call
  (testing "(+ 1.0 2.0)"
    (let [hlsl (compile-and-emit '(+ 1.0 2.0) :fragment "main")]
      (is (hlsl-contains? hlsl "(1.0 + 2.0)"))
      (is (hlsl-contains? hlsl "return")))))

(deftest test-let-binding
  (testing "let 绑定"
    (let [hlsl (compile-and-emit '(let* [x 42.0] x) :fragment "main")]
      (is (hlsl-contains? hlsl "const float x = 42.0"))
      (is (hlsl-contains? hlsl "return x;")))))

(deftest test-float4-constructor
  (testing "向量构造 float4"
    (let [hlsl (compile-and-emit '(float4 1.0 2.0 3.0 4.0) :fragment "main")]
      (is (hlsl-contains? hlsl "float4(1.0, 2.0, 3.0, 4.0)"))
      (is (hlsl-contains? hlsl "return")))))

(deftest test-if-statement
  (testing "if 表达式"
    (let [hlsl (compile-and-emit '(if true 1.0 0.0) :fragment "main")]
      (is (hlsl-contains? hlsl "if (true)"))
      (is (hlsl-contains? hlsl "return 1.0"))
      (is (hlsl-contains? hlsl "else")))))

#_(deftest test-while-loop
  (testing "while loop with mutable variable"
    (let [hlsl (compile-and-emit '(loop* [i 0.0] (if (< i 10.0) (recur (+ i 1.0)) i)) :fragment "main")]
      (is (hlsl-contains? hlsl "while"))
      (is (hlsl-contains? hlsl "i = "))
      (is (hlsl-contains? hlsl "return i;")))))

(deftest test-function-definition
  (testing "顶层函数定义"
    (let [hlsl (compile-and-emit '(def square (fn* [^:float x] (* x x))) :fragment "square")]
      (is (hlsl-contains? hlsl "float square(float x)"))
      (is (hlsl-contains? hlsl "return (x * x)"))
      ;; 不再检查 void main()
      )))


(deftest test-vertex-shader-entry
  (testing "顶点着色器入口点生成"
    (let [hlsl (compile-and-emit '(top.kzre.homunculus.backend.shader.dsl/defshader
                                    :vertex vs-main
                                    [^:SV_Position ^:float4 pos]
                                    pos)
                                 :vertex "vs-main")]
      ;; 只有一个参数且为 SV_Position，不会生成 VSInput
      (is (str/includes? hlsl "struct VSOutput"))
      (is (str/includes? hlsl "SV_POSITION"))
      (is (str/includes? hlsl "float4 vs_main(float4 pos : SV_Position)")))))

(deftest test-vertex-shader-with-input
  (testing "顶点着色器入口点生成（包含额外输入）"
    (let [hlsl (compile-and-emit '(top.kzre.homunculus.backend.shader.dsl/defshader
                                    :vertex vs-main
                                    [^:SV_Position ^:float4 pos
                                     ^:TEXCOORD0  ^:float2 uv]
                                    pos)
                                 :vertex "vs-main")]
      (is (str/includes? hlsl "struct VSInput"))
      (is (str/includes? hlsl "float2 uv : TEXCOORD0;"))
      (is (str/includes? hlsl "struct VSOutput"))
      (is (str/includes? hlsl "output.pos = vs_main(input.pos, input.uv);")))))