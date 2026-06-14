(ns top.kzre.homunculus.backend.hlsl.lambert-test
  "兰伯特光照端到端测试：顶点着色器 + 片段着色器 → 完整 HLSL 程序。"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir1.forms]
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

(defn compile-and-emit [form entries]
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

(deftest lambert-full-test
  (testing "兰伯特光照着色器（顶点 + 片段 + uniform）"
    (let [form '(do
                  ;; 顶点着色器：直接输出位置
                  (top.kzre.homunculus.backend.shader.dsl/defshader :vertex vs-main
                                                                    [^:SV_Position ^:float4 pos]
                                                                    pos)
                  ;; 全局 uniform：光线方向
                  (def ^:uniform lightDir (float3 0.0 0.0 -1.0))
                  ;; 片段着色器：接收位置，计算漫反射
                  (top.kzre.homunculus.backend.shader.dsl/defshader :fragment ps-main
                                                                    [^:SV_POSITION ^:float4 pos]
                                                                    (let* [normal (float3 0.0 0.0 1.0)
                                                                           diff   (max (dot normal lightDir) 0.0)]
                                                                      (float4 diff diff diff 1.0))))
          hlsl (compile-and-emit form [{:stage :vertex   :fn-name "vs-main"}
                                       {:stage :fragment :fn-name "ps-main"}])]
      ;; 顶点着色器
      (is (str/includes? hlsl "float4 vs_main(float4 pos : SV_Position)"))
      (is (str/includes? hlsl "struct VSInput"))
      (is (str/includes? hlsl "struct VSOutput"))
      (is (str/includes? hlsl "output.pos = vs_main(input.pos);"))
      ;; uniform 声明
      (is (str/includes? hlsl "uniform float3 lightDir = float3(0.0, 0.0, -1.0);"))
      ;; 片段着色器
      (is (str/includes? hlsl "float4 main(float4 pos : SV_POSITION) : SV_TARGET"))
      (is (str/includes? hlsl "return ps_main(pos);"))
      (is (str/includes? hlsl "const float3 normal = float3(0.0, 0.0, 1.0);"))
      (is (str/includes? hlsl "max(dot(normal, lightDir), 0.0)"))
      (is (str/includes? hlsl "float4(diff, diff, diff, 1.0)")))))