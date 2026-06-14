(ns top.kzre.homunculus.backend.hlsl.instancing-test
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

(defn compile-instancing [form]
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
                   [{:stage :vertex :fn-name "vs-main"
                     :input-params [{:name "pos" :type "float4" :semantic "SV_POSITION"}
                                    {:name "instanceID" :type "uint" :semantic "SV_INSTANCEID"}]
                     :output-params [{:name "pos" :type "float4" :semantic "SV_POSITION"}]}])))

(deftest test-instancing-vertex
  (testing "顶点着色器实例化入口（系统值分离）"
    (let [hlsl (compile-instancing
                 '(top.kzre.homunculus.backend.shader.dsl/defshader :vertex vs-main
                                                                    [^:SV_Position ^:float4 pos
                                                                     ^:SV_InstanceID ^:uint instanceID]
                                                                    pos))]
      (is (str/includes? hlsl "struct VSInput {"))
      (is (str/includes? hlsl "float4 pos : SV_POSITION;"))
      ;; 系统值不应出现在输入结构体中
      (is (not (str/includes? (second (re-find #"struct VSInput \{\n([^}]*)\}" hlsl)) "SV_INSTANCEID")))
      ;; 入口签名应包含系统值作为额外参数
      (is (str/includes? hlsl "VSOutput main(VSInput input, uint instanceID : SV_INSTANCEID)"))
      (is (str/includes? hlsl "output.pos = vs_main(input.pos, instanceID);")))))