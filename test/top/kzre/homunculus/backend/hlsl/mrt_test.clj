(ns top.kzre.homunculus.backend.hlsl.mrt-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.backend.hlsl.integration-test :refer [elab-config hlsl-frontend full-builtins]]))

(def hlsl-backend-real (hlsl-backend/->HLSLBackend))

(defn compile-mrt [form]
  (let [expanded   (walk/macroexpand-all form)
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        mutable    (mut/analyze elaborated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/run checked-fn :frontend hlsl-frontend)
        typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
        checked    (check/check-program typed {:backend hlsl-backend-real})]
    (emit/generate checked hlsl-backend-real
                   [{:stage :fragment :fn-name "ps-main"
                     :input-params  [{:name "pos" :type "float4" :semantic "SV_POSITION"}]
                     :output-params [{:name "color0" :type "float4" :semantic "SV_TARGET0"}
                                     {:name "color1" :type "float4" :semantic "SV_TARGET1"}]}])))

(deftest test-mrt-two-targets
  (testing "两个渲染目标的片段着色器入口"
    (let [hlsl (compile-mrt
                 '(top.kzre.homunculus.backend.shader.dsl/defshader :fragment ps-main
                                                                    [^:SV_POSITION ^:float4 pos ^:out ^:float4 color0 ^:out ^:float4 color1]
                                                                    (set! color0 pos)
                                                                    (set! color1 (float4 1.0 0.0 0.0 1.0))))]
      (is (str/includes? hlsl "struct PSOutput {"))
      (is (str/includes? hlsl "float4 color0 : SV_TARGET0;"))
      (is (str/includes? hlsl "float4 color1 : SV_TARGET1;"))
      (is (str/includes? hlsl "ps_main(pos, output.color0, output.color1);"))
      (is (str/includes? hlsl "return output;")))))