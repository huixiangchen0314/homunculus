(ns top.kzre.homunculus.backend.hlsl.compute-test
  "计算着色器入口生成测试"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [top.kzre.homunculus.core.ir1.api :as ir1]
            [top.kzre.homunculus.core.ir2.api :as ir2]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.infer.api :as infer]
            [top.kzre.homunculus.core.types.constraint.api :as solve]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.check.api :as check]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.backend.hlsl.integration-test :refer [elab-config hlsl-frontend full-builtins]]))

(def hlsl-backend-real (hlsl-backend/->HLSLBackend))

(defn compile-compute [form]
  (let [expanded   (walk/macroexpand-all form)
        ir1-root (ir1/->ir1 expanded)
        ir2-root (ir2/->ir2 ir1-root)
        no-recur   (mapv recur-elim/eliminate ir2-root)
        elaborated (elaborate/elaborate no-recur elab-config)
        mutable    (mut/analyze elaborated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/infer checked-fn (infer/make-context hlsl-frontend))
        solved      (solve/process inferred (solve/make-context hlsl-frontend nil))
        checked    (check/check solved (check/make-context hlsl-backend-real))]
    (emit/generate checked hlsl-backend-real
                   [{:stage :compute :fn-name "cs-main"
                     :output-params [{:numthreads [8 8 1]}]}])))

(deftest test-compute-shader-entry
  (testing "计算着色器入口"
    (let [hlsl (compile-compute
                 '(top.kzre.homunculus.backend.shader.dsl/defshader :compute cs-main [] nil))]
      ;; 计算着色器入口无返回值，但测试仅验证入口包装生成
      (is (str/includes? hlsl "[numthreads(8, 8, 1)]"))
      (is (str/includes? hlsl "void main()"))
      (is (str/includes? hlsl "cs_main();")))))