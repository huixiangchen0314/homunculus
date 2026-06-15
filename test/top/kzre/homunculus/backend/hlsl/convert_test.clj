(ns top.kzre.homunculus.backend.hlsl.convert-test
  "隐式类型转换测试：验证 int/float 混合运算自动插入转换。"
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

;; ── 复制必要的配置（与 integration_test 保持一致）──
(def elab-config
  (reify elab-cfg/IElaborateConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (allow-return-closure? [_] false)
    (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (should-inline? [_ _ _] true)))

(def hlsl-frontend (hlsl-front/->HLSLFrontend))
;; ⚠️ 使用真实后端，而非 Mock
(def hlsl-backend-real (hlsl-backend/->HLSLBackend))
(def full-builtins (merge {} hlsl-front/builtins))

(defn compile-and-emit-convert [form entries]
  "使用真实 HLSL 后端进行编译，以激活类型转换。"
  (let [expanded   (walk/macroexpand-all form)
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        mutable    (mut/analyze elaborated)
        checked-fn (builtin/check mutable full-builtins)
        inferred   (infer/infer checked-fn :frontend hlsl-frontend)
        typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
        ;; 关键：这里传入真实后端，使 type-conversion 生效
        checked    (check/check-program typed {:backend hlsl-backend-real})]
    (emit/generate checked hlsl-backend-real entries)))

;; ── 测试 ──
(deftest test-int-plus-float
  (testing "int + float 自动转换为 float"
    (let [hlsl (compile-and-emit-convert '(+ 1 2.0) [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "(float)1"))
      (is (str/includes? hlsl "return")))))

(deftest test-float-plus-int
  (testing "float + int 自动转换为 float"
    (let [hlsl (compile-and-emit-convert '(+ 2.0 1) [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "(float)1"))
      (is (str/includes? hlsl "return")))))

(deftest test-float-vector-with-int
  (testing "float2 构造中 int 自动转换"
    (let [hlsl (compile-and-emit-convert '(float2 1 2.0) [{:stage :fragment :fn-name "frag"}])]
      (is (str/includes? hlsl "(float)1"))
      (is (str/includes? hlsl "float2((float)1, 2.0)"))
      (is (str/includes? hlsl "return")))))