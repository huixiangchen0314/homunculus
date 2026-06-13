(ns top.kzre.homunculus.backend.shader.ho-test
  "高阶消除集成测试：验证 reduce/map 等从 Clojure 源码到 HLSL 代码的完整流程。"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
            [top.kzre.homunculus.core.types.recur-elim.methods]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.elaborate.methods]
            [top.kzre.homunculus.core.types.elaborate.protocol :as elab-cfg]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.mutability.core :as mut]
            [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.test-utils :refer :all]))

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
(def ho-elim-config (hlsl-front/->HLSLHoElimConfig))

(defn compile-ho-and-emit [form entries]
  (let [expanded   (macroexpand-deep form)
        ir1-root   (ir1/->ir1 expanded)
        ir2-roots  (ir2/lower [ir1-root])
        no-recur   (mapv recur-elim/eliminate ir2-roots)
        elaborated (elaborate/elaborate no-recur elab-config)
        ;; 1. 类型推断
        inferred   (infer/run elaborated :frontend hlsl-frontend)
        typed      (typed/type-check inferred :frontend hlsl-frontend :builtins full-builtins)
        ;; 2. 高阶消除
        ho-elimed  (ho-elim/process typed ho-elim-config)
        ;; 3. 再次类型推断（为展开的新节点赋予类型）
        ;; TODO 增加一个轻量级的局部传播 pass 完成展开节点的类型推断
        typed2     (typed/type-check ho-elimed :frontend hlsl-frontend :builtins full-builtins)
        ;; 4. 后续 Pass
        mutable    (mut/analyze typed2)
        checked-fn (builtin/check mutable full-builtins)
        checked    (check/check-program checked-fn {:backend mock-backend})
        roots      (if (some #(= (ir2p/kind %) :define) checked)
                     checked
                     (let [body (if (= 1 (count checked))
                                  (first checked)
                                  (m/->BlockNode checked nil nil nil))]
                       [(m/->DefineNode (symbol (:fn-name (first entries)))  ;; 使用 entries 中的名字
                                        (m/->LambdaNode [] body [] nil nil nil nil)
                                        nil nil nil nil)]))]
    (emit/generate roots hlsl-backend-inst entries)))  ;; 传递 entries

(defn hlsl-contains? [hlsl substr]
  (str/includes? hlsl substr))

;; ══════════════════════════════════════════════
;; 集成测试
;; ══════════════════════════════════════════════
(deftest test-reduce-expansion
  (testing "reduce 在固定向量上完全展开"
    (let [hlsl (compile-ho-and-emit '(reduce + 0.0 [1.0 2.0 3.0]) [{:stage :fragment :fn-name "main"}])]
      (is (hlsl-contains? hlsl "return"))
      (is (hlsl-contains? hlsl "1.0"))
      (is (hlsl-contains? hlsl "2.0"))
      (is (hlsl-contains? hlsl "3.0")))))

;(deftest test-map-expansion
;  (testing "map 在固定向量上展开为逐元素调用"
;    (let [hlsl (compile-ho-and-emit '(map abs [1.0 -2.0]) :fragment "main")]
;      (is (hlsl-contains? hlsl "return"))
;      (is (hlsl-contains? hlsl "abs(1.0)"))
;      (is (hlsl-contains? hlsl "abs(-2.0)")))))
;
;(deftest test-reduce-dynamic-vector-throws
;  (testing "reduce 在非字面量向量上抛出异常"
;    (is (thrown? clojure.lang.ExceptionInfo
;                 (compile-ho-and-emit '(let* [v [1.0 2.0]] (reduce + 0.0 v)) :fragment "main")))))